{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

import Prelude hiding (maximum, minimum)

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Exception (finally)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
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
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.RequestLogger qualified as Wai
import Network.Wai.Middleware.Cors qualified as Cors
import Options.Applicative
    ( auto, help, long, metavar, option, strOption, value )
import Options.Applicative qualified as Opt
import System.Log.FastLogger qualified as Logger
import System.Random (randomRIO)
import Text.Printf (printf)

import UnliftIO.Async qualified as Async

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
  ~(FlexiBool devRequestLogger) <- option auto
    ( long "dev-request-logger"
   <> value (FlexiBool True)
   <> metavar "BOOL"
   <> help "Enable request logging in development mode"
    )
  return $ do
    (logger, cleanup) <- Logger.newFastLogger $ Logger.LogStdout Logger.defaultBufSize
    let logLn msg = logger $ msg <> "\n"
    manager <- HTTP.newManager HTTP.defaultManagerSettings
    let
      runApp name app = do
        let ctx = AppCtx
              { appLogger = \msg -> logLn $ "[" <> name <> "] " <> msg
              , appManager = manager
              }
        runReaderT app ctx
    ttHandle <- newTTHandle
    (waitActivity, reportActivity) <- newSignal
    runApp "Startup Trigger" $ requestBlocks miningClientUrl allChains 2
    checkIdleTriggerPeriod idleTriggerPeriod
    let -- Because of the way chainweb-mining-client produces blocks and the way
        -- it interacts with Chainweb's chain braiding, we need to adjust the
        -- idleTriggerPeriod to get approximately the desired period.
        periodicBlocksDelay = idleTriggerPeriod * 0.616
        executeAsync threads = do
          let threadIOs = threads <&> \(name, threadApp) -> do
                logLn $ "Starting " <> name
                name <$ runApp name threadApp
          (_, name) <- Async.waitAnyCancel =<< mapM Async.async threadIOs
          logLn $ "Thread " <> name <> " has exited"
        name =: thread = (name, thread)

    flip finally cleanup $ executeAsync $
      [ "Transaction Proxy" =: transactionProxy ProxyArgs
          { transactionBatchPeriod
          , chainwebServiceEndpoint
          , listenPort
          , defaultConfirmation
          , ttHandle
          , devRequestLogger
          }
      ] ++ [
        "Confirmation Trigger" =: transactionWorker TransactionWorkerArgs
          { miningClientUrl
          , confirmationTriggerPeriod
          , ttHandle
          , reportActivity
          , miningCooldown
          }
        | not disableConfirmation
      ] ++ [
        "Periodic Trigger" =: periodicBlocks
          miningClientUrl
          periodicBlocksDelay
          waitActivity
        | not disableIdle
      ]
  where
    run m = join $ Opt.execParser $ Opt.info
      (Opt.helper <*> m)
      (Opt.fullDesc <> Opt.progDesc "Chainweb Mining Trigger")
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
  , devRequestLogger :: Bool
  }

transactionProxy :: ProxyArgs -> App ()
transactionProxy  ProxyArgs{..} = do
  Logg logg <- askLogg
  manager <- asks appManager
  let setFormat s = if devRequestLogger then s else s
        { Wai.outputFormat = Wai.Apache Wai.FromHeader
        }
  logMiddleware <- liftIO $ Wai.mkRequestLogger $ Wai.defaultRequestLoggerSettings
    & setFormat
    & \s -> s { Wai.destination = Wai.Callback logg }

  let opts = S.defaultOptions
        { S.verbose = 0
        , S.settings = Warp.setPort listenPort Warp.defaultSettings
        }
  logg $ "Listening on port " <> Logger.toLogStr (show listenPort)
  liftIO $ S.scottyOpts opts $ do
    S.middleware $ logMiddleware
    S.middleware $ Cors.cors . const . Just $ Cors.simpleCorsResourcePolicy
      { Cors.corsRequestHeaders = Cors.simpleHeaders
      }
    S.post (S.regex "/chainweb/0.0/([0-9a-zA-Z\\-\\_]+)/chain/([0-9]+)/pact/api/v1/send") $ do
      networkId <- S.captureParam "1"
      chainId <- S.captureParam "2"
      accepted <- proxySend manager chainwebServiceEndpoint networkId chainId
      let demand = defaultConfirmation
      liftIO $ when accepted $ do
        if demand > 0
        then pushTransaction ttHandle transactionBatchPeriod (read chainId) demand
        else logg $
          "Not requesting blocks due to confirmation demand = " <> fromString (show demand)

data TransactionWorkerArgs = TransactionWorkerArgs
  { miningClientUrl :: String
  , confirmationTriggerPeriod :: Double
  , ttHandle :: TTHandle
  , reportActivity :: ReportSignal
  , miningCooldown :: Double
  }

transactionWorker :: TransactionWorkerArgs -> App ()
transactionWorker TransactionWorkerArgs{..} = forever $ do
  (chains, confirmations) <- liftIO $ waitTrigger confirmationTriggerPeriod ttHandle
  liftIO $ report reportActivity
  forM_ (NE.nonEmpty chains) $ \neChains ->
    replicateM_ confirmations $ do
      requestBlocks miningClientUrl neChains 1
      liftIO $ threadDelay $ round $ miningCooldown * 1_000_000

periodicBlocks :: String -> Double -> WaitSignal -> App ()
periodicBlocks miningClientUrl delay waitActivity = forever $ do
  let sleep = threadDelay $ round $ delay * 1_000_000
  liftIO (Async.race (wait waitActivity) sleep) >>= \case
    Left () -> return () -- Network not idle
    Right () -> do
      chainid <- liftIO $ randomRIO (0, 19)
      requestBlocks miningClientUrl (NE.singleton chainid) 1

-------------------------------------------------------------------------------

data AppCtx = AppCtx
  { appLogger :: Logger.FastLogger
  , appManager :: HTTP.Manager
  }

type App = ReaderT AppCtx IO

logg :: Logger.LogStr -> App ()
logg msg = asks appLogger >>= \logger -> liftIO $ logger msg

newtype Logg = Logg (forall m. MonadIO m => Logger.LogStr -> m ())

askLogg :: App Logg
askLogg = asks appLogger <&> \l -> Logg $ \msg -> liftIO $ l msg

proxySend :: HTTP.Manager -> String -> String -> String -> S.ActionM Bool
proxySend manager chainwebServiceEndpoint networkId chainId = do
  let path = "/chainweb/0.0/" ++ networkId ++ "/chain/" ++ chainId ++ "/pact/api/v1/send"
  body <- S.body
  request <- S.request
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


requestBlocks :: String -> NE.NonEmpty Int -> Int -> App ()
requestBlocks miningClientUrl chainids count = do
  manager <- asks appManager
  response <- liftIO $ do
    request <- HTTP.parseRequest (miningClientUrl <> "/make-blocks") <&> \r -> r
      { HTTP.method = "POST"
      , HTTP.requestHeaders = [("Content-Type", "application/json")]
      , HTTP.requestBody = HTTP.RequestBodyLBS $ A.encode $
          A.Object $ flip foldMap chainids $ \c -> fromString (show c) A..= count
      }
    HTTP.httpLbs request manager
  let desc = printf "Requested %s on %s" blocks chains where
        blocks = show count ++ if count == 1 then " block" else " blocks"
        chains = case chainids of
          c NE.:| [] -> "chain " ++ show c
          cs -> "chains " ++ show (NE.toList cs)
  logg $ Logger.toLogStr $ if HTTP.responseStatus response == HTTP.status200
    then desc
    else desc ++ ", failed to make blocks: " ++ show response

allChains :: NE.NonEmpty Int
allChains = NE.fromList [0..19]

newtype WaitSignal = WaitSignal {wait :: IO ()}
newtype ReportSignal = ReportSignal {report :: IO ()}

newSignal :: IO (WaitSignal, ReportSignal)
newSignal = newEmptyMVar <&> \mvar ->
  ( WaitSignal $ takeMVar mvar
  , ReportSignal $ void $ tryPutMVar mvar ()
  )

-------------------------------------------------------------------------------

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