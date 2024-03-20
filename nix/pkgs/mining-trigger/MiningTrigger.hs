{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Prelude hiding (maximum, minimum)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async qualified as Async
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
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
import Options.Applicative qualified as Opt
import System.Random (randomRIO)
import Text.Printf (printf)
import Web.Scotty qualified as S

-------------------------------------------------------------------------------

import TriggerState

main :: IO ()
main = run $ do
  miningClientUrl <- Opt.strOption
    ( Opt.long "mining-client-url"
   <> Opt.value "http://localhost:1790"
   <> Opt.metavar "URL"
   <> Opt.help "URL of the mining client to trigger block production"
    )
  chainwebServiceEndpoint <- Opt.strOption
    ( Opt.long "chainweb-service-endpoint"
   <> Opt.value "http://localhost:1848"
   <> Opt.metavar "URL"
   <> Opt.help "URL of the chainweb service to proxy transactions to"
    )
  idleTriggerPeriod <- Opt.option Opt.auto
    ( Opt.long "idle-trigger-period"
   <> Opt.value 10
   <> Opt.metavar "SECONDS"
   <> Opt.help "Period in seconds to produce another block height when idle"
    )
  transactionTriggerPeriod <- Opt.option Opt.auto
    ( Opt.long "confirmation-trigger-period"
   <> Opt.value 1
   <> Opt.metavar "SECONDS"
   <> Opt.help "Period in seconds to trigger consecutive confirmation blocks"
    )
  transactionBatchPeriod <- Opt.option Opt.auto
    ( Opt.long "transaction-batch-period"
   <> Opt.value 0.05
   <> Opt.metavar "SECONDS"
   <> Opt.help "Period in seconds to wait for batching transactions"
    )
  defaultConfirmation <- Opt.option Opt.auto
    ( Opt.long "confirmation-count"
   <> Opt.value 5
   <> Opt.metavar "BLOCKS"
   <> Opt.help "Default number of confirmations for transactions"
    )
  listenPort <- Opt.option Opt.auto
    ( Opt.long "port"
   <> Opt.value 1791
   <> Opt.metavar "PORT"
   <> Opt.help "Port to listen for transaction requests"
    )
  return $ do
    ttHandle <- newTTHandle
    requestBlocks miningClientUrl "Startup Trigger" allChains 2
    let -- Because of the way chainweb-mining-client produces blocks and the way
        -- it interacts with Chainweb's chain braiding, we need to adjust the
        -- idleTriggerPeriod to get approximately the desired period.
        periodicBlocksDelay = idleTriggerPeriod * 0.616
    executeAsync
      [ "Transaction Proxy" <$ transactionProxy ProxyArgs
          { transactionBatchPeriod
          , chainwebServiceEndpoint
          , listenPort
          , defaultConfirmation
          , ttHandle
          }
      , "Transaction Worker" <$ transactionWorker
          miningClientUrl
          transactionTriggerPeriod
          ttHandle
      , "Periodic Trigger" <$ periodicBlocks miningClientUrl periodicBlocksDelay
      ]
  where
    run m = join $ Opt.execParser $ Opt.info
      (Opt.helper <*> m)
      (Opt.fullDesc <> Opt.progDesc "Chainweb Mining Trigger")
    executeAsync threads = do
      (_, name) <- Async.waitAnyCancel =<< mapM Async.async threads
      putStrLn $ "Thread " ++ name ++ " has exited"

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
    proxySend chainwebServiceEndpoint networkId chainId
    liftIO $ pushTransaction ttHandle transactionBatchPeriod (read chainId) defaultConfirmation

transactionWorker :: String -> Double -> TTHandle -> IO ()
transactionWorker miningClientUrl triggerPeriod tt = forever $ do
  (chains, confirmations) <- waitTrigger triggerPeriod tt
  forM_ (NE.nonEmpty chains) $ \neChains ->
    requestBlocks miningClientUrl "Transaction Worker" neChains confirmations

periodicBlocks :: String -> Double -> IO ()
periodicBlocks miningClientUrl delay = forever $ do
  chainid <- randomRIO (0, 19) :: IO Int
  requestBlocks miningClientUrl "Periodic Trigger" (NE.singleton chainid) 1
  threadDelay $ round $ delay * 1_000_000

-------------------------------------------------------------------------------

proxySend :: String -> String -> String -> S.ActionM ()
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
  forM_ (HTTP.responseHeaders response) $ \(name, value) -> do
    let conv = T.decodeUtf8 . BL.fromStrict
    unless (name `elem` [ "Transfer-Encoding", "Access-Control-Allow-Origin"]) $
      S.setHeader (conv $ CI.original name) (conv value)
  S.raw $ HTTP.responseBody response


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
