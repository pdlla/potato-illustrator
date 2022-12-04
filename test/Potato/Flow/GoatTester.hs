{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Potato.Flow.GoatTester where

import           Relude                            hiding (empty, fromList, first)

import           Test.HUnit

import           Reflex
import           Reflex.Test.Host

import           Potato.Flow
import           Potato.Flow.DebugHelpers

import qualified Data.Text                         as T
import           Data.These
import Data.Default


type GoatTesterTrackingState = (Text, Int, Int)

data GoatTesterRecord = GoatTesterRecord {
  _goatTesterFailureRecord_trackingState :: GoatTesterTrackingState
  , _goatTesterFailureRecord_failureMessage :: Maybe Text
}

data GoatTesterState = GoatTesterState {
    _goatTesterState_goatState :: GoatState
    , _goatTesterState_rawOperationCount :: Int
    , _goatTesterState_marker :: Text
    , _goatTesterState_rawOperationCountSinceLastMarker :: Int
    , _goatTesterState_records :: [GoatTesterRecord]
  }

instance Default GoatTesterState where
  def = GoatTesterState {
      _goatTesterState_goatState = makeGoatState (emptyOwlPFState, emptyControllerMeta)
      , _goatTesterState_rawOperationCount = 0
      , _goatTesterState_marker = "__START__"
      , _goatTesterState_rawOperationCountSinceLastMarker = 0
      , _goatTesterState_records = []
    }

newtype GoatTesterT m a = GoatTesterT { unGoatTester :: StateT GoatTesterState m a } deriving (Functor, Applicative, Monad)
type GoatTester a = GoatTesterT Identity a


runCommand :: (Monad m) => GoatCmd -> GoatTesterT m ()
runCommand cmd = GoatTesterT $ modify $
  \gts@GoatTesterState {..} -> gts {
      _goatTesterState_goatState = foldGoatFn cmd _goatTesterState_goatState
      , _goatTesterState_rawOperationCount = _goatTesterState_rawOperationCount + 1
      , _goatTesterState_rawOperationCountSinceLastMarker = _goatTesterState_rawOperationCountSinceLastMarker + 1
    }

setMarker :: (Monad m) => Text -> GoatTesterT m ()
setMarker marker = GoatTesterT $ modify $
  \gts -> gts {
      _goatTesterState_marker = marker
      , _goatTesterState_rawOperationCountSinceLastMarker = 0
    }

getTrackingState :: (Monad m) => GoatTesterT m GoatTesterTrackingState
getTrackingState = GoatTesterT $ do
  GoatTesterState {..} <- get
  return $ (_goatTesterState_marker, _goatTesterState_rawOperationCountSinceLastMarker, _goatTesterState_rawOperationCount)

verifyState' :: (Monad m) => (GoatState -> Maybe Text) -> GoatTesterT m Bool
verifyState' fn = GoatTesterT $ do
  gts <- get
  ts <- unGoatTester getTrackingState
  let mf = fn (_goatTesterState_goatState gts)
  let
    record = GoatTesterRecord {
        _goatTesterFailureRecord_trackingState = ts
        , _goatTesterFailureRecord_failureMessage = mf
      }
  put $ gts { _goatTesterState_records = record : _goatTesterState_records gts }
  return $ isJust mf

verifyState :: (Monad m) => (GoatState -> Maybe Text) -> GoatTesterT m ()
verifyState f = verifyState f >> return ()

--verifyStateFatal :: (GoatState -> Maybe Text) -> GoatTesterT m ()
--verifyStateFatal = undefined

runGoatTesterT :: (Monad m) => GoatState -> GoatTesterT m a -> m [GoatTesterRecord]
runGoatTesterT gs m = do
  gts <- execStateT (unGoatTester m) $ def { _goatTesterState_goatState = gs }
  return $ reverse $ _goatTesterState_records gts

runGoatTester :: GoatState -> GoatTester a -> [GoatTesterRecord]
runGoatTester gs m = runIdentity $ runGoatTesterT gs m
