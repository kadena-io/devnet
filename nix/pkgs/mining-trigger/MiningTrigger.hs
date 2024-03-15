{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

import Prelude hiding (maximum, minimum)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.Aeson qualified as A
import Data.Generics.Labels ()
import Data.IORef
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes)
import Data.Strict qualified as Strict
import Data.String (fromString)
import Data.Text.Lazy.Encoding qualified as T
import GHC.Generics (Generic)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Middleware.RequestLogger qualified as Wai
import Network.Wai.Middleware.Cors qualified as Cors
import Options.Applicative qualified as Opt
import System.Clock qualified as Clock
import System.Random (randomRIO)
import Text.Printf (printf)
import Web.Scotty qualified as S

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

type Confirmations = Int
type ChainId = Int

data TransactionTriggerState = TTS
  { chainMap :: (M.Map ChainId Confirmations)
  , scheduledTrigger :: Strict.Maybe Clock.TimeSpec
  , pendingFlush :: Bool
  } deriving (Show, Generic)

emptyTTS :: TransactionTriggerState
emptyTTS = TTS M.empty Strict.Nothing False

atStrict :: Ord k => k -> Lens' (M.Map k v) (Maybe v)
atStrict k f = M.alterF f k

insertTransaction ::
  ChainId -> Clock.TimeSpec -> Confirmations ->
  TransactionTriggerState -> TransactionTriggerState
insertTransaction chainId latest confirmations tts =
  tts & #chainMap . atStrict chainId %~ Just . maybe pending (+pending)
      & #scheduledTrigger %~ Strict.Just . Strict.maybe latest (min latest)
      & #pendingFlush .~ True
  where pending = confirmations + 1 -- Since we need to flush as well

type Demands = ([ChainId], Confirmations)

popPending :: Clock.TimeSpec -> Double -> TransactionTriggerState ->
  (Demands, TransactionTriggerState)
popPending now confirmationPeriod tts = if Strict.Just now < scheduledTrigger tts
  then (([],0), tts)
  else
    let
      confirmationsDemand = if pendingFlush tts then 2 else 1
      nextTrigger = now + Clock.fromNanoSecs (round $ confirmationPeriod * 1_000_000_000)
      positive = anon 0 (<=0)
      (chains, poppedMap) = flip M.traverseMaybeWithKey (chainMap tts) $ \chainId c ->
        forOf positive (Just c) $ \confirmations ->
          ([chainId], confirmations - confirmationsDemand)
      newTTS = tts
        { chainMap = poppedMap
        , scheduledTrigger =
            Strict.toStrict $ nextTrigger <$ guard (not $ M.null poppedMap)
        , pendingFlush = False
        }
    in ((chains, confirmationsDemand), newTTS)

popPendingIO :: IORef TransactionTriggerState -> Double -> IO Demands
popPendingIO ref confirmationPeriod = do
  now <- Clock.getTime Clock.Monotonic
  atomicModifyIORef' ref $ \state ->
    let (demands, state') = popPending now confirmationPeriod state
    in (state', demands)

data TTHandle = TTHandle
  { ttStateRef :: IORef TransactionTriggerState
  , ttSignal :: MVar ()
  }

newTTHandle :: IO TTHandle
newTTHandle = TTHandle <$> newIORef emptyTTS <*> newEmptyMVar

pushTransaction :: TTHandle -> Double -> Int -> Int -> IO ()
pushTransaction TTHandle{..} batchPeriod chainId pending = do
  let batchPeriodTimeSpec = Clock.fromNanoSecs $ round $ batchPeriod * 1_000_000_000
  latest <- (+ batchPeriodTimeSpec) <$> Clock.getTime Clock.Monotonic
  atomicModifyIORef' ttStateRef $ \state ->
    (insertTransaction chainId latest pending state, ())
  putMVar ttSignal ()

transactionWorker :: String -> Double -> TTHandle -> IO ()
transactionWorker miningClientUrl triggerPeriod TTHandle{..} = forever $ do
  (chains, confirmations) <- waitTrigger
  forM_ (NE.nonEmpty chains) $ \neChains ->
    requestBlocks miningClientUrl "Transaction Worker" neChains confirmations
  where
    waitTrigger = do
      tts <- readIORef ttStateRef
      let waitScheduled = (Strict.fromStrict $ scheduledTrigger tts) <&> \scheduled -> do
            now <- Clock.getTime Clock.Monotonic
            let delay = max 0 $ Clock.toNanoSecs $ scheduled - now
            threadDelay $ fromIntegral $ delay `div` 1_000
          waitSignal = takeMVar ttSignal
          wakeUps = catMaybes
            [ waitScheduled
            , Just waitSignal
            ]
      void $ Async.waitAnyCancel =<< mapM Async.async wakeUps
      popPendingIO ttStateRef triggerPeriod

allChains :: NE.NonEmpty Int
allChains = NE.fromList [0..19]

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
   <> Opt.help "Period in seconds to trigger block production"
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
    ( Opt.long "default-confirmation"
   <> Opt.value 4
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
      , "Periodic Trigger" <$ periodicBlocks miningClientUrl idleTriggerPeriod
      ]
  where
    run m = join $ Opt.execParser $ Opt.info
      (Opt.helper <*> m)
      (Opt.fullDesc <> Opt.progDesc "Chainweb Mining Trigger")
    executeAsync threads = do
      (_, name) <- Async.waitAnyCancel =<< mapM Async.async threads
      putStrLn $ "Thread " ++ name ++ " has exited"

periodicBlocks :: String -> Int -> IO ()
periodicBlocks miningClientUrl delay = forever $ do
  chainid <- randomRIO (0, 19) :: IO Int
  requestBlocks miningClientUrl "Periodic Trigger" (NE.singleton chainid) 1
  threadDelay $ delay * 1_000_000

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
