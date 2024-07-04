-- TODO Rename to ShapeText.hs

{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-record-wildcards #-}

module Potato.Flow.Controller.Manipulator.BoxText (
  TextInputState(..)
  , lBox_to_boxLabelBox
  , shrink_lBox_no_negative
  , getSBoxTextBox

  -- TODO rename to boxTextAreaTextImpl and boxLabelTextImpl
  , boxTextImpl
  , boxLabelImpl

  , makeShapeTextHandler
  , makeShapeLabelHandler

  -- exposed for testing
  , makeTextInputState
  , mouseText

) where

import           Relude

import Potato.Flow.Controller.Manipulator.TextInputState
import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.Manipulator.Common
import           Potato.Flow.Math
import           Potato.Flow.Serialization.Snake
import           Potato.Flow.Types
import Potato.Flow.Owl
import Potato.Flow.Llama
import           Potato.Flow.Preview

import           Data.Default
import           Data.Dependent.Sum                        (DSum ((:=>)))
import qualified Data.IntMap                               as IM
import qualified Data.Sequence                             as Seq
import qualified Text.Pretty.Simple as Pretty
import qualified Data.Text.Lazy as LT

getSBox :: CanvasSelection -> (REltId, SBox)
getSBox selection = case superOwl_toSElt_hack sowl of
  SEltBox sbox -> (rid, sbox)
  selt -> error $ "expected SBox, got " <> show selt
  where
    sowl = selectionToSuperOwl selection
    rid = _superOwl_id sowl

-- | shrink an LBox uniformly in each direction, but don't allow it to become negative
shrink_lBox_no_negative :: LBox -> Int -> Int -> LBox
shrink_lBox_no_negative (LBox (V2 x y) (V2 w h)) dw dh = LBox (V2 nx ny) (V2 nw nh) where
  (nx, nw) = if w <= 2*dw
    then if w <= dw
      -- prioritize shrinking from the right
      then (x, 0)
      else (x + (w - dw), 0)
    else (x+dw, w-2*dw)
  (ny, nh) = if h <= 2*dh
    then if h <= dh
      -- prioritize shrinking from the bottom
      then (y, 0)
      else (y + (h - dh), 0)
    else (y+dh, h-2*dh)


getSBoxTextBox :: SBox -> CanonicalLBox
getSBoxTextBox sbox = r where
  CanonicalLBox fx fy box' = canonicalLBox_from_lBox $ _sBox_box sbox
  r = CanonicalLBox fx fy $  if sBoxType_hasBorder (_sBox_boxType sbox)
    then shrink_lBox_no_negative box' 1 1
    else box'


makeBoxTextController :: Text -> Text -> Controller
makeBoxTextController orig new = CTagBoxText :=> (Identity $ CBoxText {
      _cBoxText_deltaText = (orig, new)
    })

boxTextImpl :: TextImpl SBox
boxTextImpl = TextImpl {
    _textImpl_mustGetOwlItem = getSBox
    --, _textImpl_updateTextInputStateWithOwlItem = updateTextInputStateWithSBox
    , _textImpl_owlItemText = _sBoxText_text . _sBox_text
    , _textImpl_owlItemBox = getSBoxTextBox
    , _textImpl_owlItemAlignment = _textStyle_alignment . _sBoxText_style . _sBox_text
    , _textImpl_inputOwlItemZipper = inputBoxTextZipper
    , _textImpl_makeController = makeBoxTextController
  }

makeTextInputState :: REltId -> SBox -> RelMouseDrag -> TextInputState
makeTextInputState = makeOwlItemTextInputState boxTextImpl

inputBoxText :: TextInputState -> SuperOwl -> KeyboardKey -> (TextInputState, Maybe Llama)
inputBoxText tais sowl kk = inputOwlItem boxTextImpl tais sowl kk

lBox_to_boxLabelBox :: LBox -> LBox
lBox_to_boxLabelBox lbx = r where
  CanonicalLBox _ _ (LBox (V2 x y) (V2 w _)) = canonicalLBox_from_lBox lbx
  width = max 0 (w - 2)
  r = LBox (V2 (x+1) y) (V2 width 1)

makeBoxLableController :: Text -> Text -> Controller
makeBoxLableController orig new = CTagBoxLabelText :=> (Identity $ CMaybeText (DeltaMaybeText (if orig == "" then Nothing else Just orig, if new == "" then Nothing else Just new)))

boxLabelImpl :: TextImpl SBox
boxLabelImpl = TextImpl {
    _textImpl_mustGetOwlItem = getSBox
    , _textImpl_owlItemText = fromMaybe "" . _sBoxTitle_title . _sBox_title
    , _textImpl_owlItemBox = canonicalLBox_from_lBox . lBox_to_boxLabelBox . _sBox_box
    , _textImpl_owlItemAlignment = _sBoxTitle_align . _sBox_title
    , _textImpl_inputOwlItemZipper = inputSingleLineZipper
    , _textImpl_makeController = makeBoxLableController
  }

-- WIP SHAPETEXTHANDLER STARTS HERE
data ShapeTextHandler o = ShapeTextHandler {
    -- TODO Delete this
    _shapeTextHandler_isActive      :: Bool
    
    , _shapeTextHandler_state       :: TextInputState
    -- TODO you can prob delete this now, we don't persist state between sub handlers in this case
    , _shapeTextHandler_prevHandler :: SomePotatoHandler
    , _shapeTextHandler_undoFirst   :: Bool

    , _shapeTextHandler_commitOnMouseUp :: Bool

    , _shapeTextHandler_textImpl :: TextImpl o
  }


makeShapeTextHandler :: TextImpl o -> Bool -> SomePotatoHandler -> CanvasSelection -> RelMouseDrag -> ShapeTextHandler o
makeShapeTextHandler textImpl commit prev selection rmd = ShapeTextHandler {
      _shapeTextHandler_isActive = False
      , _shapeTextHandler_state = uncurry (makeOwlItemTextInputState textImpl) (_textImpl_mustGetOwlItem textImpl $ selection) rmd
      , _shapeTextHandler_prevHandler = prev
      , _shapeTextHandler_undoFirst = False
      , _shapeTextHandler_commitOnMouseUp = commit
      , _shapeTextHandler_textImpl = textImpl
    }

updateShapeTextHandlerState :: TextImpl o -> Bool -> CanvasSelection -> ShapeTextHandler o -> ShapeTextHandler o
updateShapeTextHandlerState textImpl reset selection tah@ShapeTextHandler {..} = r where
  nextstate = updateOwlItemTextInputState textImpl reset selection _shapeTextHandler_state
  r = tah {
    _shapeTextHandler_state = nextstate
    , _shapeTextHandler_undoFirst = if reset
      then False
      else _shapeTextHandler_undoFirst
  }

instance PotatoHandler (ShapeTextHandler o) where
  pHandlerName _ = handlerName_shapeText
  pHandlerDebugShow ShapeTextHandler {..} = LT.toStrict $ Pretty.pShowNoColor _shapeTextHandler_state
  pHandleMouse tah' phi@PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = let
      (rid, sbox) = getSBox _potatoHandlerInput_canvasSelection
      tah@ShapeTextHandler {..} = updateShapeTextHandlerState _shapeTextHandler_textImpl False _potatoHandlerInput_canvasSelection tah'
    in case _mouseDrag_state of
      MouseDragState_Down -> r where
        clickInside = does_lBox_contains_XY (_textInputState_box _shapeTextHandler_state) _mouseDrag_to
        newState = mouseText _shapeTextHandler_state rmd
        r = if clickInside
          then Just $ def {
              _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
                  _shapeTextHandler_isActive = True
                  , _shapeTextHandler_state = newState
                }
            }
          -- pass the input on to the base handler (so that you can interact with BoxHandler mouse manipulators too)
          else pHandleMouse _shapeTextHandler_prevHandler phi rmd

      -- TODO drag select text someday
      MouseDragState_Dragging -> Just $ captureWithNoChange tah

      MouseDragState_Up -> r where

        -- if box is not text box, convert to text box and set undoFirst to True
        oldbt = _sBox_boxType $ sbox
        istext = sBoxType_isText oldbt
        newbt = make_sBoxType (sBoxType_hasBorder oldbt) True

        -- if it's not a text box, convert it to one (remember that this gets called from pHandleMouse with MouseDragState_Up in BoxHandler)
        r = if not istext
          then Just $ def {
              _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
                  _shapeTextHandler_isActive = False    
                }
              -- TODO change this to just PO_Start
              -- the issue here is when you undo, it becomes not a text box, so you need to make sure to convert it to a text box in the preview operation (actually to do that, there's no point in converting it here really)
              -- NOTE if we PO_Start/_shapeTextHandler_undoFirst = True we will undo the conversion to text box :(. It's fine, just permanently convert it to a text box, NBD
              -- also NOTE that this will not undo the text box conversion if you cancel this handler, it will just permanently be a text box now.
              -- NOTE this creates a weird undo operation that just converts from text to not text which is weird
              , _potatoHandlerOutput_action = HOA_Preview $ Preview PO_StartAndCommit $ makePFCLlama . OwlPFCManipulate $ IM.fromList [(rid, CTagBoxType :=> Identity (CBoxType (oldbt, newbt)))]

            }
          else Just $ def {
              _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
                  _shapeTextHandler_isActive = False
                  , _shapeTextHandler_commitOnMouseUp = False
                }
              , _potatoHandlerOutput_action = if _shapeTextHandler_commitOnMouseUp then HOA_Preview Preview_Commit else HOA_Nothing
            }
      MouseDragState_Cancelled -> Just $ captureWithNoChange tah

  pHandleKeyboard tah' PotatoHandlerInput {..} (KeyboardData k _) = case k of
    KeyboardKey_Esc -> Just $ def { _potatoHandlerOutput_nextHandler = Just (_shapeTextHandler_prevHandler tah') }
    -- TODO should only capture stuff caught by inputBoxTextZipper

    _ -> Just r where
      -- this regenerates displayLines unecessarily but who cares
      tah = updateShapeTextHandlerState (_shapeTextHandler_textImpl tah') False _potatoHandlerInput_canvasSelection tah'
      sowl = selectionToSuperOwl _potatoHandlerInput_canvasSelection

      -- TODO decide what to do with mods

      (nexttais, mllama) = inputBoxText (_shapeTextHandler_state tah) sowl k
      r = def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
              _shapeTextHandler_state  = nexttais
              , _shapeTextHandler_undoFirst = case mllama of
                Nothing -> _shapeTextHandler_undoFirst tah
                --Nothing -> False -- this variant adds new undo point each time cursoer is moved
                Just _  -> True
            }
          -- TODO do a Preview_Cancel if we reverted back to original text
          -- TODO we want to PO_Continue here, but we don't have a good place to commit right now as there's no explicit cancel for us to Preview_Commit
          , _potatoHandlerOutput_action = maybe HOA_Nothing (HOA_Preview . Preview (previewOperation_fromUndoFirst (_shapeTextHandler_undoFirst tah))) mllama

        }

  -- TODO do you need to reset _shapeTextHandler_prevHandler as well?
  pRefreshHandler tah PotatoHandlerInput {..} = if Seq.null (unCanvasSelection _potatoHandlerInput_canvasSelection)
    then Nothing -- selection was deleted or something
    else if rid /= (_textInputState_rid $ _shapeTextHandler_state tah)
      then Nothing -- selection was change to something else
      else case selt of
        SEltBox sbox -> if not $ sBoxType_isText (_sBox_boxType sbox)
          then Nothing -- SEltBox type changed to non-text
          -- TODO this needs to merge the TextZipper if change came due to remote event
          else Just $ SomePotatoHandler $ updateShapeTextHandlerState (_shapeTextHandler_textImpl tah) True _potatoHandlerInput_canvasSelection tah
        _ -> Nothing
      where
        sowl = selectionToSuperOwl _potatoHandlerInput_canvasSelection
        rid = _superOwl_id sowl
        selt = superOwl_toSElt_hack sowl

  pRenderHandler tah' phi@PotatoHandlerInput {..} = r where
    tah = updateShapeTextHandlerState (_shapeTextHandler_textImpl tah') False _potatoHandlerInput_canvasSelection tah'
    btis = _shapeTextHandler_state tah
    r = pRenderHandler (_shapeTextHandler_prevHandler tah) phi <> makeTextHandlerRenderOutput btis

  -- TODO set properly (_shapeTextHandler_isActive checks mouse activity, but we have more subtle notions of active now)
  pIsHandlerActive tah = if _shapeTextHandler_isActive tah then HAS_Active_Mouse else HAS_Active_Keyboard






-- TODO get rid of the o, it's possible but you need to refactor all the methods to do the SomeTextImpl sorta thing, no big deal just keep the `o` lol...
data ShapeLabelHandler o = ShapeLabelHandler {
    _shapeLabelHandler_active      :: Bool
    -- NOTE some fields in here are ignored or interpreted differently from ShapeTextHandler
    , _shapeLabelHandler_state       :: TextInputState
    , _shapeLabelHandler_prevHandler :: SomePotatoHandler
    , _shapeLabelHandler_undoFirst   :: Bool
    , _shapeLabelHandler_textLabelImpl :: TextImpl o
  }

makeShapeLabelInputState :: TextImpl o -> REltId -> o -> RelMouseDrag -> TextInputState
makeShapeLabelInputState textImpl = makeOwlItemTextInputState textImpl

makeShapeLabelHandler :: TextImpl o -> SomePotatoHandler -> CanvasSelection -> RelMouseDrag -> ShapeLabelHandler o
makeShapeLabelHandler textImpl prev selection rmd = ShapeLabelHandler {
      _shapeLabelHandler_active = False
      , _shapeLabelHandler_state = uncurry (makeShapeLabelInputState textImpl) (_textImpl_mustGetOwlItem textImpl selection) rmd
      , _shapeLabelHandler_prevHandler = prev
      , _shapeLabelHandler_undoFirst = False
      , _shapeLabelHandler_textLabelImpl = textImpl

    }

updateShapeLabelHandlerState :: Bool -> CanvasSelection -> ShapeLabelHandler o -> ShapeLabelHandler o
updateShapeLabelHandlerState reset selection tah@ShapeLabelHandler {..} = r where
  nextstate = updateOwlItemTextInputState _shapeLabelHandler_textLabelImpl reset selection _shapeLabelHandler_state
  r = tah {
    _shapeLabelHandler_state = nextstate
    , _shapeLabelHandler_undoFirst = if reset
      then False
      else _shapeLabelHandler_undoFirst
  }


inputShapeLabel :: TextImpl o -> TextInputState -> SuperOwl -> KeyboardKey -> (TextInputState, Maybe Llama)
inputShapeLabel textImpl tais sowl kk = inputOwlItem textImpl tais sowl kk


-- | just a helper for pHandleMouse
handleMouseDownOrFirstUpForShapeLabelHandler :: ShapeLabelHandler o -> PotatoHandlerInput -> RelMouseDrag -> Bool -> Maybe PotatoHandlerOutput
handleMouseDownOrFirstUpForShapeLabelHandler tah@ShapeLabelHandler {..} phi@PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) isdown = r where
  clickInside = does_lBox_contains_XY (_textInputState_box _shapeLabelHandler_state) _mouseDrag_to
  newState = mouseText _shapeLabelHandler_state rmd
  r = if clickInside
    then Just $ def {
        _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
            _shapeLabelHandler_active = isdown
            , _shapeLabelHandler_state = newState
          }
      }
    -- pass the input on to the base handler (so that you can interact with BoxHandler mouse manipulators too)
    else pHandleMouse _shapeLabelHandler_prevHandler phi rmd


instance PotatoHandler (ShapeLabelHandler o) where
  pHandlerName _ = handlerName_shapeLabel
  pHandlerDebugShow ShapeLabelHandler {..} = LT.toStrict $ Pretty.pShowNoColor _shapeLabelHandler_state

  -- UNTESTED
  pHandleMouse tah' phi@PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = let
      tah@ShapeLabelHandler {..} = updateShapeLabelHandlerState False _potatoHandlerInput_canvasSelection tah'
    in case _mouseDrag_state of


      MouseDragState_Down -> handleMouseDownOrFirstUpForShapeLabelHandler tah phi rmd True

      -- TODO drag select text someday
      MouseDragState_Dragging -> Just $ captureWithNoChange tah

      MouseDragState_Up -> if not _shapeLabelHandler_active
        then handleMouseDownOrFirstUpForShapeLabelHandler tah phi rmd False
        else Just $ def {
            _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
                _shapeLabelHandler_active = False
              }
          }

      MouseDragState_Cancelled -> Just $ captureWithNoChange tah

  pHandleKeyboard tah' PotatoHandlerInput {..} (KeyboardData k _) = case k of
    KeyboardKey_Esc -> Just $ def { _potatoHandlerOutput_nextHandler = Just (_shapeLabelHandler_prevHandler tah') }
    -- TODO should only capture stuff caught by inputSingleLineZipper
    _ -> Just r where
      -- this regenerates displayLines unecessarily but who cares
      tah@ShapeLabelHandler {..} = updateShapeLabelHandlerState False _potatoHandlerInput_canvasSelection tah'
      sowl = selectionToSuperOwl _potatoHandlerInput_canvasSelection

      -- TODO decide what to do with mods

      (nexttais, mllama) = inputShapeLabel _shapeLabelHandler_textLabelImpl _shapeLabelHandler_state sowl k
      r = def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler tah {
              _shapeLabelHandler_state  = nexttais
              , _shapeLabelHandler_undoFirst = case mllama of
                Nothing -> _shapeLabelHandler_undoFirst
                --Nothing -> False -- this variant adds new undo point each time cursoer is moved
                Just _  -> True
            }
          , _potatoHandlerOutput_action = maybe HOA_Nothing (HOA_Preview . Preview (previewOperation_fromUndoFirst _shapeLabelHandler_undoFirst)) mllama
        }

  -- UNTESTED
  -- TODO do you need to reset _shapeLabelHandler_prevHandler as well?
  pRefreshHandler tah PotatoHandlerInput {..} = if Seq.null (unCanvasSelection _potatoHandlerInput_canvasSelection)
    then Nothing -- selection was deleted or something
    else if rid /= (_textInputState_rid $ _shapeLabelHandler_state tah)
      then Nothing -- selection was change to something else
      else case selt of
        SEltBox sbox -> if sBoxType_hasBorder (_sBox_boxType sbox)
          -- TODO this needs to merge the TextZipper if change came due to remote event
          then Just $ SomePotatoHandler $ updateShapeLabelHandlerState True _potatoHandlerInput_canvasSelection tah
          -- SEltBox type changed to non-text
          else Nothing
        _ -> Nothing
      where
        sowl = selectionToSuperOwl _potatoHandlerInput_canvasSelection
        rid = _superOwl_id sowl
        selt = superOwl_toSElt_hack sowl

  pRenderHandler tah' phi@PotatoHandlerInput {..} = r where
    tah = updateShapeLabelHandlerState False _potatoHandlerInput_canvasSelection tah'
    btis = _shapeLabelHandler_state tah
    r = pRenderHandler (_shapeLabelHandler_prevHandler tah) phi <> makeTextHandlerRenderOutput btis

  -- TODO set properly
  pIsHandlerActive tah = if _shapeLabelHandler_active tah then HAS_Active_Mouse else HAS_Active_Keyboard