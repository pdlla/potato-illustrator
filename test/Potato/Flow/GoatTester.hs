{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}


module Potato.Flow.GoatTester where

import           Relude                   hiding (empty, first, fromList)

import           Test.Hspec

import           Potato.Flow
import           Potato.Flow.DebugHelpers

import           Data.Default
import qualified Data.IntMap              as IM
import qualified Data.List                as L
import           Data.Maybe               (fromJust)
import qualified Data.Sequence            as Seq
import qualified Data.Text                as T
import           Data.Tuple.Extra


goatTesterInitialScreenSize :: V2 Int
goatTesterInitialScreenSize = V2 100 100


type GoatTesterTrackingState = (Text, Int, Int) -- (marker, operations since start of test, operations since last marker)

-- TODO include ref to rendered canvas and then you can render it on failure cases!!
data GoatTesterRecord = GoatTesterRecord {
  _goatTesterRecord_trackingState    :: GoatTesterTrackingState
  , _goatTesterRecord_failureMessage :: Maybe Text
  , _goatTesterRecord_description    :: Text
} deriving (Show)

data GoatTesterState = GoatTesterState {
    _goatTesterState_goatState                          :: GoatState
    , _goatTesterState_rawOperationCount                :: Int

    -- TODO consider making marker more complex
    , _goatTesterState_marker                           :: Text

    , _goatTesterState_rawOperationCountSinceLastMarker :: Int
    , _goatTesterState_records                          :: [GoatTesterRecord]
  } deriving (Show)

instance Default GoatTesterState where
  def = GoatTesterState {
      _goatTesterState_goatState = makeGoatState goatTesterInitialScreenSize (emptyOwlPFState, emptyControllerMeta)
      , _goatTesterState_rawOperationCount = 0
      , _goatTesterState_marker = "__START__"
      , _goatTesterState_rawOperationCountSinceLastMarker = 0
      , _goatTesterState_records = []
    }

newtype GoatTesterT m a = GoatTesterT { unGoatTesterT :: StateT GoatTesterState m a } deriving (Functor, Applicative, Monad, MonadState GoatTesterState)
type GoatTester a = GoatTesterT Identity a


runEndo :: (Monad m) => (GoatState -> GoatState) -> GoatTesterT m ()
runEndo fn = GoatTesterT $ modify $
  \gts@GoatTesterState {..} -> gts {
      _goatTesterState_goatState = fn _goatTesterState_goatState
      , _goatTesterState_rawOperationCount = _goatTesterState_rawOperationCount + 1
      , _goatTesterState_rawOperationCountSinceLastMarker = _goatTesterState_rawOperationCountSinceLastMarker + 1
    }


getGoatState :: (Monad m) => GoatTesterT m GoatState
getGoatState = GoatTesterT $ do
  GoatTesterState {..} <- get
  return _goatTesterState_goatState

getOwlPFState :: (Monad m) => GoatTesterT m OwlPFState
getOwlPFState = GoatTesterT $ do
  GoatTesterState {..} <- get
  return $ goatState_pFState _goatTesterState_goatState

getTrackingState :: (Monad m) => GoatTesterT m GoatTesterTrackingState
getTrackingState = GoatTesterT $ do
  GoatTesterState {..} <- get
  return $ (_goatTesterState_marker, _goatTesterState_rawOperationCountSinceLastMarker, _goatTesterState_rawOperationCount)

putRecord :: (Monad m) => Text -> Maybe Text -> GoatTesterT m ()
putRecord desc mf = GoatTesterT $ do
  gts <- get
  ts <- unGoatTesterT getTrackingState
  let record = GoatTesterRecord {
      _goatTesterRecord_trackingState = ts
      , _goatTesterRecord_failureMessage = mf
      , _goatTesterRecord_description = desc
    }
  put $ gts { _goatTesterState_records = record : _goatTesterState_records gts }

setMarker :: (Monad m) => Text -> GoatTesterT m ()
setMarker marker = GoatTesterT $ do
  modify $ \gts -> gts {
      _goatTesterState_marker = marker
      , _goatTesterState_rawOperationCountSinceLastMarker = 0
    }

verify :: (Monad m) => Text -> Maybe Text -> GoatTesterT m ()
verify desc mf = GoatTesterT $ do
  unGoatTesterT $ putRecord desc mf

verifyBool :: (Monad m) => Text -> Bool -> GoatTesterT m ()
verifyBool desc b = GoatTesterT $ do
  unGoatTesterT $ putRecord desc (if b then Nothing else Just "failed")

verifyEqual :: (Monad m, Show a, Eq a) => Text -> a -> a -> GoatTesterT m ()
verifyEqual desc a b = GoatTesterT $ do
  unGoatTesterT $ putRecord desc (if a == b then Nothing else Just $ "expected: " <> show a <> "\ngot: " <> show b)

verifyState' :: (Monad m) => Text -> (GoatState -> Maybe Text) -> GoatTesterT m Bool
verifyState' desc fn = GoatTesterT $ do
  gts <- get
  let mf = fn (_goatTesterState_goatState gts)
  unGoatTesterT $ putRecord desc mf
  return $ isJust mf

verifyState :: (Monad m) => Text -> (GoatState -> Maybe Text) -> GoatTesterT m ()
verifyState desc f = verifyState' desc f >> return ()

--verifyStateFatal :: (GoatState -> Maybe Text) -> GoatTesterT m ()
--verifyStateFatal = undefined

failWithMessage :: (Monad m) => Text -> GoatTesterT m ()
failWithMessage = putRecord "failed: " . Just

runGoatTesterT :: (Monad m) => GoatState -> GoatTesterT m a -> m [GoatTesterRecord]
runGoatTesterT gs m = do
  gts <- execStateT (unGoatTesterT m) $ def { _goatTesterState_goatState = gs }
  return $ reverse $ _goatTesterState_records gts

runGoatTester :: GoatState -> GoatTester a -> [GoatTesterRecord]
runGoatTester gs m = runIdentity $ runGoatTesterT gs m

hSpecGoatTesterWithOwlPFState_verbose :: OwlPFState -> GoatTester a -> SpecWith ()
hSpecGoatTesterWithOwlPFState_verbose pfs m = do
  let
    rslt' = runGoatTester (makeGoatState (V2 100 100) (pfs, emptyControllerMeta)) m
    rslt = L.groupBy (\a b -> fst3 (_goatTesterRecord_trackingState a) == fst3 (_goatTesterRecord_trackingState b)) rslt'
  forM_ rslt $ \gtss -> case gtss of
    [] -> return ()
    (x:xs) -> do
      describe (T.unpack (fst3 (_goatTesterRecord_trackingState x))) $ forM_ (x:xs) $ \GoatTesterRecord {..} -> do
        it (T.unpack _goatTesterRecord_description) $ case _goatTesterRecord_failureMessage of
          Nothing -> return ()
          Just x' -> expectationFailure (T.unpack x')


hSpecGoatTesterWithOwlPFState_quiet :: OwlPFState -> GoatTester a -> SpecWith ()
hSpecGoatTesterWithOwlPFState_quiet pfs m = do
  let
    rslt' = runGoatTester (makeGoatState (V2 100 100) (pfs, emptyControllerMeta)) m
    rslt = L.groupBy (\a b -> fst3 (_goatTesterRecord_trackingState a) == fst3 (_goatTesterRecord_trackingState b)) rslt'
  -- TODO better test case name  (pass it in)
  -- TODO accumulate and output more information on failure as single failure
  it "passes" $ forM_ rslt $ \gtss -> case gtss of
    [] -> return ()
    (x:xs) -> do
      forM_ (x:xs) $ \GoatTesterRecord {..} -> do
        case _goatTesterRecord_failureMessage of
          Nothing -> return ()
          Just x' -> expectationFailure (T.unpack x')

hSpecGoatTesterWithOwlPFState :: OwlPFState -> GoatTester a -> SpecWith ()
hSpecGoatTesterWithOwlPFState = hSpecGoatTesterWithOwlPFState_verbose

-- state getter helpers
getOwlCount :: (Monad m) => GoatTesterT m Int
getOwlCount = do
  pfs <- getOwlPFState
  return . IM.size . _owlTree_mapping . hasOwlTree_owlTree $ pfs

maybeGetMostRecentlyCreatedOwl' :: OwlPFState -> Maybe SuperOwl
maybeGetMostRecentlyCreatedOwl' pfs = maximumBy' (\s1 s2 -> compare (_superOwl_id s1) (_superOwl_id s2)) $ owliterateall (hasOwlTree_owlTree pfs)

mustGetMostRecentlyCreatedOwl :: (Monad m) => GoatTesterT m SuperOwl
mustGetMostRecentlyCreatedOwl = do
  pfs <- getOwlPFState
  return $ fromJust $ maybeGetMostRecentlyCreatedOwl' pfs

getLayersState :: (Monad m) => GoatTesterT m LayersState
getLayersState = GoatTesterT $ do
  gts <- get
  return $ _goatState_layersState $ _goatTesterState_goatState gts

getHandlerRenderOutput :: (Monad m) => GoatTesterT m HandlerRenderOutput
getHandlerRenderOutput = GoatTesterT $ do
  gs <- fmap _goatTesterState_goatState get
  return $ pRenderHandler (_goatState_handler gs) (potatoHandlerInputFromGoatState gs)

-- operation helpers

setTool :: (Monad m) => Tool -> GoatTesterT m ()
setTool tool = runEndo (endoGoatCmdSetTool tool)

getTool :: (Monad m) => GoatTesterT m Tool
getTool = do
  gs <- getGoatState
  return $ goatState_selectedTool gs

setFocusArea :: (Monad m) => GoatFocusedArea -> GoatTesterT m ()
setFocusArea fa = runEndo $ endoGoatCmdSetFocusedArea fa


setDefaultParams :: (Monad m) => SetPotatoDefaultParameters -> GoatTesterT m ()
setDefaultParams params = runEndo $ endoGoatCmdSetDefaultParams params


addFolder :: (Monad m) => Text -> GoatTesterT m ()
addFolder name = runEndo $ endoGoatCmdNewFolder name

canvasMouseToScreenMouse :: GoatState -> XY -> XY
canvasMouseToScreenMouse gs pos = owlPFState_fromCanvasCoordinates (goatState_pFState gs) pos + _goatState_pan gs

mouse :: (Monad m) => Bool -> Bool -> [KeyModifier] -> MouseButton -> XY -> GoatTesterT m ()
mouse isCanvas isRelease modifiers button pos = do
  gts <- get
  runEndo $ endoGoatCmdMouse $ LMouseData {
    _lMouseData_position       = if isCanvas then canvasMouseToScreenMouse (_goatTesterState_goatState gts ) pos else pos
    , _lMouseData_isRelease    = isRelease
    , _lMouseData_button       = button
    , _lMouseData_modifiers    = modifiers
    , _lMouseData_isLayerMouse = not isCanvas
  }

canvasMouseDown :: (Monad m) =>  (Int, Int) -> GoatTesterT m ()
canvasMouseDown (x,y) = mouse True False [] MouseButton_Left (V2 x y)

canvasMouseUp :: (Monad m) =>  (Int, Int) -> GoatTesterT m ()
canvasMouseUp (x,y) = mouse True True [] MouseButton_Left (V2 x y)

canvasMouseDownUp :: (Monad m) =>  (Int, Int) -> GoatTesterT m ()
canvasMouseDownUp (x,y) = do
  canvasMouseDown (x,y)
  canvasMouseUp (x,y)

layerMouseDown :: (Monad m) =>  (Int, Int) -> GoatTesterT m ()
layerMouseDown (x,y) = mouse False False [] MouseButton_Left (V2 x y)

layerMouseUp :: (Monad m) =>  (Int, Int) -> GoatTesterT m ()
layerMouseUp (x,y) = mouse False True [] MouseButton_Left (V2 x y)

layerMouseDownUp :: (Monad m) =>  (Int, Int) -> GoatTesterT m ()
layerMouseDownUp (x,y) = do
  layerMouseDown (x,y)
  layerMouseUp (x,y)


layerMouseDownWithModifier :: (Monad m) =>  (Int, Int) -> KeyModifier -> GoatTesterT m ()
layerMouseDownWithModifier (x,y) modifier = mouse False False [modifier] MouseButton_Left (V2 x y)

layerMouseUpWithModifier :: (Monad m) =>  (Int, Int) -> KeyModifier -> GoatTesterT m ()
layerMouseUpWithModifier (x,y) modifier = mouse False True [modifier] MouseButton_Left (V2 x y)

layerMouseDownUpWithModifier :: (Monad m) =>  (Int, Int) -> KeyModifier -> GoatTesterT m ()
layerMouseDownUpWithModifier (x,y) modifier = do
  layerMouseDownWithModifier (x,y) modifier 
  layerMouseUpWithModifier (x,y) modifier


data LayerMouseOp = LMO_Collapse | LMO_Hide | LMO_Lock | LMO_Normal 
  | LMO_DropInFolder Int -- desired position relative to depth
  deriving (Show, Eq)

layerMouseRel' :: (Monad m) => Bool -> LayerMouseOp -> Int -> Int -> GoatTesterT m ()
layerMouseRel' isup op y' depth = do
  scrollPos <- (_layersState_scrollPos . _goatState_layersState . _goatTesterState_goatState) <$> get
  let
    x' = case op of
      LMO_Collapse -> 0
      LMO_Hide     -> 1
      LMO_Lock     -> 2
      LMO_Normal -> 3
      LMO_DropInFolder rel -> rel
    y = y' - scrollPos
    x = x' + depth
  putRecord "validate y pos" (if y < 0 then Just ("y: " <> show y <> " outside of scroll position: " <> show scrollPos) else Nothing)
  if isup
    then layerMouseUp (x, y)
    else layerMouseDown (x, y)

layerMouseDownRel :: (Monad m) => LayerMouseOp -> Int -> Int -> GoatTesterT m ()
layerMouseDownRel = layerMouseRel' False

layerMouseUpRel :: (Monad m) => LayerMouseOp -> Int -> Int -> GoatTesterT m ()
layerMouseUpRel = layerMouseRel' True

layerMouseDownUpRel :: (Monad m) => LayerMouseOp -> Int -> Int -> GoatTesterT m ()
layerMouseDownUpRel op y depth = do
  layerMouseDownRel op y depth
  layerMouseUpRel op y depth




pressKeyboardKey :: (Monad m) => KeyboardKey -> GoatTesterT m ()
pressKeyboardKey k = runEndo $ endoGoatCmdKeyboard (KeyboardData k [])

pressKey :: (Monad m) => Char -> GoatTesterT m ()
pressKey c = pressKeyboardKey (KeyboardKey_Char c)

pressKeys :: (Monad m) => String -> GoatTesterT m ()
pressKeys text = forM_ text $ \c -> pressKeyboardKey (KeyboardKey_Char c)

pressEscape :: (Monad m) => GoatTesterT m ()
pressEscape = pressKeyboardKey KeyboardKey_Esc

pressReturn :: (Monad m) => GoatTesterT m ()
pressReturn = pressKeyboardKey KeyboardKey_Return

pressBackspace :: (Monad m) => GoatTesterT m ()
pressBackspace = pressKeyboardKey KeyboardKey_Backspace

pressDelete :: (Monad m) => GoatTesterT m ()
pressDelete = pressKeyboardKey KeyboardKey_Delete

pressUndo :: (Monad m) => GoatTesterT m ()
pressUndo = runEndo $ endoGoatCmdKeyboard  (KeyboardData (KeyboardKey_Char 'z') [KeyModifier_Ctrl])

pressRedo :: (Monad m) => GoatTesterT m ()
pressRedo = runEndo $ endoGoatCmdKeyboard  (KeyboardData (KeyboardKey_Char 'y') [KeyModifier_Ctrl])

markSaved :: (Monad m) => GoatTesterT m ()
markSaved = runEndo $ endoGoatCmdMarkSaved ()

resizeScreen :: (Monad m) => V2 Int -> GoatTesterT m ()
resizeScreen (V2 w h) = runEndo $ endoGoatCmdSetCanvasRegionDim (V2 w h)


-- verification helpers

-- | verifies that the number of owls (elts) in the state is what is expected, includes folders in the count
verifyOwlCount :: (Monad m) => Int -> GoatTesterT m ()
verifyOwlCount expected = verifyState "verifyOwlCount" f where
  f gs = if n == expected
    then Nothing
    else Just $ "got " <> show n <> " owls, expected " <> show expected
    where n = IM.size . _owlTree_mapping . hasOwlTree_owlTree . goatState_pFState $ gs

-- | verifies that the number of selected owls (elts) is what's expected (uses regular selection, NOT canvas selection)
verifySelectionCount :: (Monad m) => Int -> GoatTesterT m ()
verifySelectionCount expected = verifyState "verifySelectionCount" f where
  f gs = if n == expected
    then Nothing
    else Just $ "got " <> show n <> " selected owls, expected " <> show expected
    where n = isParliament_length . _goatState_selection $ gs

maximumBy' :: Foldable t => (a -> a -> Ordering) -> t a -> Maybe a
maximumBy' f l = if length l == 0
  then Nothing
  else Just $ L.maximumBy f l

verifyMostRecentlyCreatedOwl' :: (Monad m) => Text -> (SuperOwl -> Maybe Text) -> GoatTesterT m ()
verifyMostRecentlyCreatedOwl' desc f = verifyState desc f' where
  f' gs = r where
    msowl = maybeGetMostRecentlyCreatedOwl' (goatState_pFState gs)
    r = case msowl of
      Nothing -> Just "failed, no 🦉s"
      Just sowl -> (\m -> "failed with message: " <> m <> "\ngot:\n" <> potatoShow (_superOwl_elt sowl)) <$> f sowl

verifyMostRecentlyCreatedOwl :: (Monad m) => (SuperOwl -> Maybe Text) -> GoatTesterT m ()
verifyMostRecentlyCreatedOwl = verifyMostRecentlyCreatedOwl' "verifyMostRecentlyCreatedOwl"

verifyMostRecentlyCreatedLine :: (Monad m) => (SAutoLine -> Maybe Text) -> GoatTesterT m ()
verifyMostRecentlyCreatedLine f = verifyMostRecentlyCreatedOwl' "verifyMostRecentlyCreatedLine" f' where
  f' sowl = case _owlItem_subItem (_superOwl_elt sowl) of
    OwlSubItemLine sline -> f sline
    x                      -> Just $ "expected SAutoLine got: " <> show x


verifyCanvasHandler :: (Monad m) => Text -> GoatTesterT m ()
verifyCanvasHandler hname = verifyState "verifyCanvasHandler" f where
  f gs = r where
    h = _goatState_handler gs
    r = if hname == pHandlerName h
      then Nothing
      else Just $ "expected " <> hname <> "got handler " <> pHandlerName h 

verifySelectionIsAndOnlyIs :: (Monad m) => Text -> (SuperOwl -> Maybe Text) -> GoatTesterT m ()
verifySelectionIsAndOnlyIs desc f = verifyState desc f' where
  f' gs = r where
    SuperOwlParliament selection = _goatState_selection gs
    sowl = Seq.index selection 0
    nselection = Seq.length selection
    r = if nselection /= 1
      then Just $ "failed, expected 1 selected 🦉, got " <> show nselection
      else (\m -> "failed with message: " <> m <> "\ngot:\n" <> potatoShow (_superOwl_elt sowl)) <$> f sowl

verifyRenderNonEmptyCount :: (Monad m) => Int -> GoatTesterT m ()
verifyRenderNonEmptyCount expected = verifyState "verifyRenderNonEmptyCount" f where
  f gs = if n == expected
    then Nothing
    else Just $ "got " <> show n <> " non-empty chars, expected " <> show expected
    where n =  renderedCanvasRegion_nonEmptyCount (_goatState_renderedCanvas gs)

verifyCharRenderedAt :: (Monad m) => V2 Int -> Maybe Char -> GoatTesterT m ()
verifyCharRenderedAt pos mchar = verifyState "verifyCharRenderedAt" f where
  f gs = if mchar == mchar'
    then Nothing
    else Just $ "expected: " <> show mchar <> "\ngot: " <> show mchar'
    where
      mchar' = case renderedCanvasRegion_getAt (_goatState_renderedCanvas gs) pos of
        (-1, _) -> Nothing
        (_, c) -> Just c


-- otheruseful stuff

-- export as part of this module becaues it's super useful
-- from https://hackage.haskell.org/package/utility-ht-0.0.16/docs/src/Data.Maybe.HT.html#toMaybe
{-# INLINE toMaybe #-}
toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True  x = Just x

alwaysFail :: (Monad m) => Text -> GoatTesterT m ()
alwaysFail msg = GoatTesterT $ do
  unGoatTesterT $ putRecord "this test always fails" (Just msg)


debugFailWithLayers :: (Monad m) => GoatTesterT m ()
debugFailWithLayers = do
  gs <- getGoatState
  putRecord "debugFailWithLayers" (Just $ potatoShow $ _goatState_layersState gs)

debugFailWithRenderedCanvas :: (Monad m) => GoatTesterT m ()
debugFailWithRenderedCanvas = do
  gs <- getGoatState
  let
    canvas = _goatState_renderedCanvas gs
  putRecord "debugFailWithRenderedCanvas" (Just $ renderedCanvasToText canvas)
