{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async qualified as Async
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.Aeson qualified as A
import Data.Functor ((<&>))
import Data.IORef
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.String (fromString)
import Data.Text.Lazy.Encoding qualified as T
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Middleware.RequestLogger qualified as Wai
import Network.Wai.Middleware.Cors qualified as Cors
import System.Random (randomRIO)
import Text.Printf (printf)
import Web.Scotty qualified as S

proxySend :: String -> String -> S.ActionM ()
proxySend networkId chainId = do
  let path = "/chainweb/0.0/" ++ networkId ++ "/chain/" ++ chainId ++ "/pact/api/v1/send"
  body <- S.body
  request <- S.request
  manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  downstreamRequest <- liftIO $ HTTP.parseRequest $ "http://localhost:1848" ++ path
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

transactionProxy :: IORef TransactionTriggerState -> IO ()
transactionProxy tts = S.scotty 1791 $ do
  S.middleware Wai.logStdoutDev
  S.middleware $ Cors.cors . const . Just $ Cors.simpleCorsResourcePolicy
    { Cors.corsRequestHeaders = Cors.simpleHeaders
    }
  S.post (S.regex "/chainweb/0.0/([0-9a-zA-Z\\-\\_]+)/chain/([0-9]+)/pact/api/v1/send") $ do
    networkId <- S.captureParam "1"
    chainId <- S.captureParam "2"
    proxySend networkId chainId
    liftIO $ insertTransactionIO tts (read chainId) 5

type PendingConfirmation = Int
type ChainId = Int

data TransactionTriggerState = TTS
  { _chainMap :: !(M.Map ChainId PendingConfirmation)
  }

insertTransaction ::
  ChainId -> PendingConfirmation ->
  TransactionTriggerState -> TransactionTriggerState
insertTransaction chainId pending (TTS m) =
  TTS $ M.alter (Just . maybe pending (max pending)) chainId m

insertTransactionIO ::
  IORef TransactionTriggerState -> ChainId -> PendingConfirmation -> IO ()
insertTransactionIO ref chainId pending = atomicModifyIORef' ref $ \state ->
  (insertTransaction chainId pending state, ())

popPending :: TransactionTriggerState -> ([ChainId], TransactionTriggerState)
popPending (TTS m) = fmap TTS $ flip M.traverseMaybeWithKey m $
  \chainId pending -> do
    ([chainId], if pending > 1 then Just $! (pending - 1) else Nothing)

popPendingIO :: IORef TransactionTriggerState -> IO [ChainId]
popPendingIO ref = atomicModifyIORef' ref $ \state ->
  let (chains, state') = popPending state in (state', chains)

transactionWorker :: IORef TransactionTriggerState -> IO ()
transactionWorker tts = forever $ do
  chains <- popPendingIO tts
  forM_ (NE.nonEmpty chains) $ \neChains ->
    requestBlocks "Transaction Worker" neChains 1
  threadDelay 1_000_000

allChains :: NE.NonEmpty Int
allChains = NE.fromList [0..19]

main :: IO ()
main = do
  transactionChan <- newIORef $ TTS M.empty
  requestBlocks "Startup Trigger" allChains 2
  executeAsync
    [ "Transaction Proxy" <$ transactionProxy transactionChan
    , "Transaction Worker" <$ transactionWorker transactionChan
    , "Periodic Trigger" <$ periodicBlocks 10
    ]
  where
    executeAsync threads = do
      (_, name) <- Async.waitAnyCancel =<< mapM Async.async threads
      putStrLn $ "Thread " ++ name ++ " has exited"

periodicBlocks :: Int -> IO ()
periodicBlocks delay = forever $ do
  chainid <- randomRIO (0, 19) :: IO Int
  requestBlocks "Periodic Trigger" (NE.singleton chainid) 1
  threadDelay $ delay * 1_000_000

requestBlocks :: String -> NE.NonEmpty Int -> Int -> IO ()
requestBlocks source chainids count = do
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  request <- HTTP.parseRequest "http://localhost:1790/make-blocks" <&> \r -> r
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
