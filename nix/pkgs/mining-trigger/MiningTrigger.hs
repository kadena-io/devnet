{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

import Prelude hiding (maximum, minimum)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.Char (toLower)
import Data.Aeson qualified as A
import Data.Generics.Labels ()
import Data.List.NonEmpty qualified as NE
import Data.String (fromString)
import Data.Text.Lazy.Encoding qualified as T
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Middleware.RequestLogger qualified as Wai
import Network.Wai.Middleware.Cors qualified as Cors
import Options.Applicative
    ( auto, help, long, metavar, option, strOption, value )
import Options.Applicative qualified as Opt
import System.Random (randomRIO)
import Text.Printf (printf)
import Web.Scotty qualified as S

-------------------------------------------------------------------------------

import TriggerState

main :: IO ()
main = run $ do
  miningClientUrl <- strOption
    ( long "mining-client-url"
   <> value "http://localhost:1790"
   <> metavar "URL"
   <> help "URL of the mining client to trigger block production"
    )
  chainwebServiceEndpoint <- strOption
    ( long "chainweb-service-endpoint"
   <> value "http://localhost:1848"
   <> metavar "URL"
   <> help "URL of the chainweb service to proxy transactions to"
    )
  idleTriggerPeriod <- option auto
    ( long "idle-trigger-period"
   <> value 10
   <> metavar "SECONDS"
   <> help "Period in seconds to produce another block height when idle"
    )
  confirmationTriggerPeriod <- option auto
    ( long "confirmation-trigger-period"
   <> value 1
   <> metavar "SECONDS"
   <> help "Period in seconds to trigger consecutive confirmation blocks"
    )
  transactionBatchPeriod <- option auto
    ( long "transaction-batch-period"
   <> value 0.05
   <> metavar "SECONDS"
   <> help "Period in seconds to wait for batching transactions"
    )
  miningCooldown <- option auto
    ( long "mining-cooldown"
   <> value 0.05
   <> metavar "SECONDS"
   <> help "The minimum time between mining requests"
    )
  defaultConfirmation <- option auto
    ( long "confirmation-count"
   <> value 5
   <> metavar "BLOCKS"
   <> help "Default number of confirmations for transactions"
    )
  listenPort <- option auto
    ( long "port"
   <> value 1791
   <> metavar "PORT"
   <> help "Port to listen for transaction requests"
    )
  ~(FlexiBool disableIdle) <- option auto
    ( long "disable-idle-worker"
   <> value (FlexiBool False)
   <> metavar "BOOL"
   <> help "Disable idle periodic block production"
    )
  ~(FlexiBool disableConfirmation) <- option auto
    ( long "disable-confirmation-worker"
   <> value (FlexiBool False)
   <> metavar "BOOL"
   <> help "Disable confirmation block production"
    )
  return $ do
    ttHandle <- newTTHandle
    (waitActivity, reportActivity) <- newSignal
    requestBlocks miningClientUrl "Startup Trigger" allChains 2
    checkIdleTriggerPeriod idleTriggerPeriod
    let -- Because of the way chainweb-mining-client produces blocks and the way
        -- it interacts with Chainweb's chain braiding, we need to adjust the
        -- idleTriggerPeriod to get approximately the desired period.
        periodicBlocksDelay = idleTriggerPeriod * 0.616
    executeAsync $
      [ "Transaction Proxy" <$ transactionProxy ProxyArgs
          { transactionBatchPeriod
          , chainwebServiceEndpoint
          , listenPort
          , defaultConfirmation
          , ttHandle
          }
      ] ++ [
        "Transaction Worker" <$ transactionWorker TransactionWorkerArgs
          { miningClientUrl
          , confirmationTriggerPeriod
          , ttHandle
          , reportActivity
          , miningCooldown
          }
        | not disableConfirmation
      ] ++ [
        "Periodic Trigger" <$ periodicBlocks
          miningClientUrl
          periodicBlocksDelay
          waitActivity
        | not disableIdle
      ]
  where
    run m = join $ Opt.execParser $ Opt.info
      (Opt.helper <*> m)
      (Opt.fullDesc <> Opt.progDesc "Chainweb Mining Trigger")
    executeAsync threads = do
      (_, name) <- Async.waitAnyCancel =<< mapM Async.async threads
      putStrLn $ "Thread " ++ name ++ " has exited"
    checkIdleTriggerPeriod p = when (p <= 0) $
      fail "Error: Idle trigger period must be positive"

-------------------------------------------------------------------------------
-- Worker Threads
-------------------------------------------------------------------------------

data ProxyArgs = ProxyArgs
  { transactionBatchPeriod :: Double
  , chainwebServiceEndpoint :: String
  , listenPort :: Int
  , defaultConfirmation :: Int
  , ttHandle :: TTHandle
  }

transactionProxy :: ProxyArgs -> IO ()
transactionProxy  ProxyArgs{..} = S.scotty listenPort $ do
  S.middleware Wai.logStdoutDev
  S.middleware $ Cors.cors . const . Just $ Cors.simpleCorsResourcePolicy
    { Cors.corsRequestHeaders = Cors.simpleHeaders
    }
  S.post (S.regex "/chainweb/0.0/([0-9a-zA-Z\\-\\_]+)/chain/([0-9]+)/pact/api/v1/send") $ do
    networkId <- S.captureParam "1"
    chainId <- S.captureParam "2"
    accepted <- proxySend chainwebServiceEndpoint networkId chainId
    let demand = defaultConfirmation
    liftIO $ when accepted $ do
      if demand > 0
      then pushTransaction ttHandle transactionBatchPeriod (read chainId) demand
      else putStrLn $
        "(Transaction Proxy) Not requesting blocks due to confirmation demand = " ++ show demand

data TransactionWorkerArgs = TransactionWorkerArgs
  { miningClientUrl :: String
  , confirmationTriggerPeriod :: Double
  , ttHandle :: TTHandle
  , reportActivity :: ReportSignal
  , miningCooldown :: Double
  }

transactionWorker :: TransactionWorkerArgs -> IO ()
transactionWorker TransactionWorkerArgs{..} = forever $ do
  (chains, confirmations) <- waitTrigger confirmationTriggerPeriod ttHandle
  report reportActivity
  forM_ (NE.nonEmpty chains) $ \neChains ->
    replicateM_ confirmations $ do
      requestBlocks miningClientUrl "Confirmation Trigger" neChains confirmations
      threadDelay $ round $ miningCooldown * 1_000_000

periodicBlocks :: String -> Double -> WaitSignal -> IO ()
periodicBlocks miningClientUrl delay waitActivity = forever $ do
  let sleep = threadDelay $ round $ delay * 1_000_000
  Async.race (wait waitActivity) sleep >>= \case
    Left () -> return () -- Network not idle
    Right () -> do
      chainid <- randomRIO (0, 19) :: IO Int
      requestBlocks miningClientUrl "Idle Trigger" (NE.singleton chainid) 1

-------------------------------------------------------------------------------

proxySend :: String -> String -> String -> S.ActionM Bool
proxySend chainwebServiceEndpoint networkId chainId = do
  let path = "/chainweb/0.0/" ++ networkId ++ "/chain/" ++ chainId ++ "/pact/api/v1/send"
  body <- S.body
  request <- S.request
  manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  downstreamRequest <- liftIO $ HTTP.parseRequest $ chainwebServiceEndpoint ++ path
  let downstreamRequest' = downstreamRequest
        { HTTP.method = "POST"
        , HTTP.requestBody = HTTP.RequestBodyLBS body
        , HTTP.requestHeaders = Wai.requestHeaders request
        }
  response <- liftIO $ HTTP.httpLbs downstreamRequest' manager
  S.status $ HTTP.responseStatus response
  forM_ (HTTP.responseHeaders response) $ \(name, val) -> do
    let conv = T.decodeUtf8 . BL.fromStrict
    unless (name `elem` [ "Transfer-Encoding", "Access-Control-Allow-Origin"]) $
      S.setHeader (conv $ CI.original name) (conv val)
  S.raw $ HTTP.responseBody response
  return $ HTTP.responseStatus response == HTTP.status200


requestBlocks :: String -> String -> NE.NonEmpty Int -> Int -> IO ()
requestBlocks miningClientUrl source chainids count = do
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  request <- HTTP.parseRequest (miningClientUrl <> "/make-blocks") <&> \r -> r
    { HTTP.method = "POST"
    , HTTP.requestHeaders = [("Content-Type", "application/json")]
    , HTTP.requestBody = HTTP.RequestBodyLBS $ A.encode $
        A.Object $ flip foldMap chainids $ \c -> fromString (show c) A..= count
    }
  response <- HTTP.httpLbs request manager
  let desc = printf "(%s) Requested %s on %s" source blocks chains where
        blocks = show count ++ if count == 1 then " block" else " blocks"
        chains = case chainids of
          c NE.:| [] -> "chain " ++ show c
          cs -> "chains " ++ show (NE.toList cs)
  putStrLn $ if HTTP.responseStatus response == HTTP.status200
    then desc
    else desc ++ ", failed to make blocks: " ++ show (response)

allChains :: NE.NonEmpty Int
allChains = NE.fromList [0..19]

newtype WaitSignal = WaitSignal {wait :: IO ()}
newtype ReportSignal = ReportSignal {report :: IO ()}

newSignal :: IO (WaitSignal, ReportSignal)
newSignal = newEmptyMVar <&> \mvar ->
  ( WaitSignal $ takeMVar mvar
  , ReportSignal $ void $ tryPutMVar mvar ()
  )


newtype FlexiBool = FlexiBool Bool

instance Read FlexiBool where
  readsPrec _ str | Just b <- match = [(FlexiBool b, "")]
    where match = case map toLower str of
            "true" -> Just True
            "false" -> Just False
            "1" -> Just True
            "0" -> Just False
            "yes" -> Just True
            "no" -> Just False
            _ -> Nothing
  readsPrec _ _ = []