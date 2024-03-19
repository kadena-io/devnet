{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module TriggerState where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar

import Control.Lens
import Control.Monad

import Data.Generics.Labels ()
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes)
import Data.Strict qualified as Strict

import GHC.Generics (Generic)

import System.Clock qualified as Clock

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

getNextTrigger :: TransactionTriggerState -> Maybe Clock.TimeSpec
getNextTrigger = Strict.fromStrict . scheduledTrigger

-------------------------------------------------------------------------------
-- IO
-------------------------------------------------------------------------------

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

popPendingIO :: IORef TransactionTriggerState -> Double -> IO Demands
popPendingIO ref confirmationPeriod = do
  now <- Clock.getTime Clock.Monotonic
  atomicModifyIORef' ref $ \state ->
    let (demands, state') = popPending now confirmationPeriod state
    in (state', demands)

waitTrigger :: Double -> TTHandle -> IO Demands
waitTrigger triggerPeriod TTHandle{..} = do
  tts <- readIORef ttStateRef
  let waitScheduled = (getNextTrigger tts) <&> \scheduled -> do
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
