{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.Controller.Manipulator.Layers (
  LayersHandler(..)
) where

import           Relude

import           Potato.Flow.Controller.Handler
import           Potato.Flow.Controller.Input
import           Potato.Flow.Controller.OwlLayers
import           Potato.Flow.Owl
import           Potato.Flow.Controller.Types
import           Potato.Flow.Math
import           Potato.Flow.Types
import           Potato.Flow.OwlWorkspace
import           Potato.Flow.OwlState

import           Data.Dependent.Sum                        (DSum ((:=>)))
import           Data.Default
import qualified Data.IntMap                    as IM
import qualified Data.Sequence                  as Seq
import Data.Sequence ((<|))
import qualified Potato.Data.Text.Zipper                          as TZ
import qualified Data.Text as T
import Data.Char (isAlphaNum)

data LayerDragState = LDS_None | LDS_Dragging | LDS_Selecting LayerEntryPos deriving (Show, Eq)

data LayerDownType = LDT_Hide | LDT_Lock | LDT_Collapse | LDT_Normal deriving (Show, Eq)


-- TODO we could probably change this to do a more efficient binary search based on position in hierarchy
doesSelectionContainREltId_linear :: REltId -> Selection -> Bool
doesSelectionContainREltId_linear rid = isJust . find (\sowl -> rid == _superOwl_id sowl) . unSuperOwlParliament


clickLayerNew :: Seq LayerEntry -> XY -> Maybe (SuperOwl, LayerDownType, Int)
clickLayerNew lentries  (V2 absx lepos) = case Seq.lookup lepos lentries of
  Nothing                      -> Nothing
  Just le -> Just . (,,absx - layerEntry_depth le) sowl $ case () of
    () | layerEntry_isFolder le && layerEntry_depth le == absx -> LDT_Collapse
    () | layerEntry_depth le + 1 == absx   -> LDT_Hide
    () | layerEntry_depth le + 2 == absx -> LDT_Lock
    () -> LDT_Normal
    where
      sowl = _layerEntry_superOwl le


data LayersHandler = LayersHandler {
    _layersHandler_dragState   :: LayerDragState
    , _layersHandler_cursorPos :: XY
    , _layersHandler_dropSpot :: Maybe OwlSpot

  }

instance Default LayersHandler where
  def = LayersHandler {
      _layersHandler_dragState = LDS_None
      , _layersHandler_cursorPos = 0
      , _layersHandler_dropSpot = Nothing
    }

handleScroll :: (PotatoHandler h) => h -> PotatoHandlerInput -> Int -> PotatoHandlerOutput
handleScroll h PotatoHandlerInput {..} scroll  = r where
  -- TODO share this code with other handler
  scrollPos = _layersState_scrollPos _potatoHandlerInput_layersState
  maxentries = 10 + (Seq.length $ _layersState_entries _potatoHandlerInput_layersState)
  newScrollPos = max 0 (min maxentries (scrollPos + scroll))
  r = def {
      _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler h
      -- TODO clamp based on number of entries
      , _potatoHandlerOutput_layersState = Just $ _potatoHandlerInput_layersState { _layersState_scrollPos = newScrollPos}
    }


resetLayersHandler :: LayersHandler -> LayersHandler
resetLayersHandler lh = lh {
    _layersHandler_dragState = LDS_None
    , _layersHandler_dropSpot = Nothing
  }


instance PotatoHandler LayersHandler where
  pHandlerName _ = handlerName_layers

  -- we incorrectly reuse RelMouseDrag for LayersHandler even though LayersHandler doesn't care about canvas pan coords
  -- pan offset should always be set to 0 in RelMouseDrag
  pHandleMouse lh@LayersHandler {..} PotatoHandlerInput {..} (RelMouseDrag MouseDrag {..}) = let

    selection = _potatoHandlerInput_selection
    ls@(LayersState _ lentries scrollPos) = _potatoHandlerInput_layersState
    pfs = _potatoHandlerInput_pFState
    owltree = (_owlPFState_owlTree pfs)

    rawleposxy@(V2 rawxoffset rawlepos) = _mouseDrag_to
    leposxy@(V2 _ lepos) = V2 rawxoffset (rawlepos + scrollPos)

    in case (_mouseDrag_state, _layersHandler_dragState) of
      (MouseDragState_Down, LDS_None) -> r where
        shift = elem KeyModifier_Shift _mouseDrag_modifiers
        (nextDragState, mNextLayerState, changes) = case clickLayerNew lentries leposxy of
          Nothing -> (LDS_None, Nothing, IM.empty)
          -- (you can only click + drag selected elements)
          Just (downsowl, ldtdown, _) -> case ldtdown of

            LDT_Normal -> if shift || (not $ doesSelectionContainREltId_linear (_superOwl_id downsowl) selection)
              -- TODO check if element is descendent of selected element and return LDS_None if so
              -- if element wasn't selected or shift is held down, enter selection mode
              then (LDS_Selecting lepos, Nothing, IM.empty)
              else (LDS_Dragging, Nothing, IM.empty)

            -- DELETE
            -- this variant denies selecting children of selected parents but not the other way around...
            -- maybe easier to deny this at a higher level rather than here.
            {-
            LDT_Normal -> if shift
              then if exclusivedescendent
                -- element is descendent of selection and therefore do not allow selecting
                then (LDS_None, Nothing, IM.empty)
                else (LDS_Selecting lepos, Nothing, IM.empty)
              else if not isselected
                then if exclusivedescendent
                  -- element is descendent of selection and therefore do not allow selecting (TODO consider alternatively, enter dragging mode)
                  then (LDS_None, Nothing, IM.empty)
                  -- enter selection mode
                  else (LDS_Selecting lepos, Nothing, IM.empty)
                -- entry dragging mode
              else (LDS_Dragging, Nothing, IM.empty)
              where
                rid = _superOwl_id downsowl
                selectionset = superOwlParliament_toOwlParliamentSet selection
                isselected = owlParliamentSet_member rid selectionset
                exclusivedescendent = owlParliamentSet_descendent owltree rid selectionset && not isselected
            -}

            LDT_Hide -> (LDS_None, Just $ toggleLayerEntry pfs ls lepos LHCO_ToggleHide, IM.empty)
            LDT_Lock -> r' where
              nextLayersState = toggleLayerEntry pfs ls lepos LHCO_ToggleLock
              hideChanges = changesFromToggleHide pfs nextLayersState lepos
              r' = (LDS_None, Just $ nextLayersState, hideChanges)
            LDT_Collapse -> (LDS_None, Just $ toggleLayerEntry pfs ls lepos LHCO_ToggleCollapse, IM.empty)

        r = Just $ def {
            _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler lh {
                _layersHandler_dragState = nextDragState
                , _layersHandler_cursorPos = _mouseDrag_to
                , _layersHandler_dropSpot = Nothing
              }
            , _potatoHandlerOutput_layersState = mNextLayerState
            , _potatoHandlerOutput_changesFromToggleHide = changes
          }
      (MouseDragState_Down, _) -> error "unexpected, _layersHandler_dragState should have been reset on last mouse up"
      (MouseDragState_Dragging, LDS_Dragging) -> r where

        -- we will always place between dropSowl and justAboveDropSowl
        mDropSowlWithOffset = do
          (downsowl, _, offset') <- clickLayerNew lentries leposxy
          return (downsowl, offset')

        mJustAboveDropSowl = do
          lentry <- case mDropSowlWithOffset of
            Nothing -> Seq.lookup (Seq.length lentries - 1) lentries
            Just _ -> Seq.lookup (lepos-1) lentries
          return $ _layerEntry_superOwl lentry


        nparentoffset = case mDropSowlWithOffset of
          Nothing -> case mJustAboveDropSowl of
            Nothing -> error "this should never happen"
            -- we are at the very bottom
            Just asowl -> rawxoffset - superOwl_depth asowl

          Just (dsowl, x) -> case mJustAboveDropSowl of
            -- we are at the very top
            Nothing -> 0
            -- limit how deep in the hierarchy we can move based on what's below the cursor
            Just asowl -> max x (superOwl_depth dsowl - superOwl_depth asowl)

        nsibling = max 0 (- (min 0 nparentoffset))


        targetspot = case mJustAboveDropSowl of
          -- we are dropping at the top of our LayerEntries
          Nothing -> OwlSpot noOwl Nothing
          Just asowl -> if nparentoffset > 0 && isOwl_isFolder asowl
            -- drop inside at the top
            then OwlSpot (_superOwl_id asowl) Nothing
            else case owlTree_findSuperOwl owltree newsiblingid of
              Nothing -> OwlSpot noOwl siblingout
              Just newsibling -> OwlSpot (superOwl_parentId newsibling) siblingout
              where
                newsiblingid = owlTree_superOwlNthParentId owltree asowl nsibling
                siblingout = case newsiblingid of
                  x | x == noOwl -> Nothing
                  x -> Just x

        r = Just $ def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler lh {
              _layersHandler_cursorPos = _mouseDrag_to
              , _layersHandler_dropSpot = Just targetspot
            }
        }

      -- TODO someday do drag for multi-select here
      (MouseDragState_Dragging, _) -> Just $ def {
          _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler lh {
              _layersHandler_cursorPos = _mouseDrag_to
              , _layersHandler_dropSpot = Nothing
            }
        }

      (MouseDragState_Up, LDS_Selecting leposdown) -> r where
        shift = elem KeyModifier_Shift _mouseDrag_modifiers
        sowl = _layerEntry_superOwl $ Seq.index lentries leposdown
        r = Just $ def {
            _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler (resetLayersHandler lh)
            , _potatoHandlerOutput_select = Just (shift, SuperOwlParliament $ Seq.singleton sowl)
          }

      -- NOTE this will not work on inherit selected children, feature or bug??
      -- we clicked and released on a selected element, enter renaming mode
      (MouseDragState_Up, LDS_Dragging) | isNothing _layersHandler_dropSpot -> case clickLayerNew lentries leposxy of
        Nothing -> error "pretty sure this should never happen "
        -- (you can only click + drag selected elements)
        Just (downsowl, ldtdown, offset) -> case ldtdown of
          LDT_Normal | offset > 0 -> r where

            -- TODO great place for TZ.selectAll when you add selection capability into TZ
            zipper = TZ.fromText $ isOwl_name downsowl

            r = Just $ setHandlerOnly LayersRenameHandler {
                _layersRenameHandler_original = resetLayersHandler lh
                , _layersRenameHandler_renaming   = downsowl
                , _layersRenameHandler_index = lepos
                , _layersRenameHandler_zipper   = zipper
              }



          _ -> Just $ setHandlerOnly (resetLayersHandler lh)




      -- TODO when we have multi-user mode, we'll want to test if the target drop space is still valid
      (MouseDragState_Up, LDS_Dragging) -> r where
        mev = do
          spot <- _layersHandler_dropSpot
          let
            -- TODO spot is invalid if it's the child of a already select elt
            isSpotValid = True
            -- TODO modify if we drag on top of existing elt
            modifiedSpot = spot
          guard isSpotValid
          return $ WSEMoveElt (modifiedSpot, superOwlParliament_toOwlParliament selection)

        r = Just $ def {
            _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler (resetLayersHandler lh)
            , _potatoHandlerOutput_pFEvent = mev
          }

      (MouseDragState_Up, LDS_None) -> Just $ setHandlerOnly lh
      (MouseDragState_Cancelled, _) -> Just $ setHandlerOnly (resetLayersHandler lh)
      _ -> error $ "unexpected mouse state passed to handler " <> show _mouseDrag_state <> " " <> show _layersHandler_dragState

  pHandleKeyboard lh phi kbd = case kbd of
    KeyboardData (KeyboardKey_Scroll scroll) _ -> Just $ handleScroll lh phi scroll
    _ -> Nothing

  --pRenderHandler lh@LayersHandler {..} PotatoHandlerInput {..} = emptyHandlerRenderOutput

  pIsHandlerActive LayersHandler {..} = _layersHandler_dragState /= LDS_None

  -- TODO generate LHRESS_ChildSelected
  pRenderLayersHandler LayersHandler {..} PotatoHandlerInput {..} = LayersViewHandlerRenderOutput newlentries where
    selection = _potatoHandlerInput_selection
    ls@(LayersState lmm lentries scrollPos) = _potatoHandlerInput_layersState
    --pfs = _potatoHandlerInput_pFState
    --owltree = (_owlPFState_owlTree pfs)

    -- TODO would also be best to cache this in LayerState since it's also used by other operations...
    selectionset = superOwlParliament_toOwlParliamentSet selection
    isSelected lentry = owlParliamentSet_member (layerEntry_rEltId lentry) selectionset
    -- perhaps linear search is faster for smaller sets though
    --isSelected lentry = doesSelectionContainREltId_linear (_superOwl_id $ _layerEntry_superOwl lentry) selection

    -- update the selected state
    mapaccumlfn_forselection mseldepth lentry = case mseldepth of
      Nothing -> normalcase
      Just x -> if layerEntry_depth lentry > x
        then (mseldepth, makelentry LHRESS_InheritSelected)
        else normalcase
      where
        -- dot depth will be filled in later
        makelentry x = LayersHandlerRenderEntryNormal x Nothing lentry
        normalcase = if isSelected lentry
          then (Just (layerEntry_depth lentry), makelentry LHRESS_Selected)
          else (Nothing, makelentry LHRESS_None)
    (_,newlentries'') = mapAccumL mapaccumlfn_forselection Nothing lentries

    -- next insert the drop spot
    newlentries' = case _layersHandler_dropSpot of
      Nothing -> newlentries''
      Just ds -> r where
        (mleftmost, samelevel) = case _owlSpot_parent ds of
            x | x == noOwl -> (maybe Nothing Just (_owlSpot_leftSibling ds), True)
            x -> case _owlSpot_leftSibling ds of
              Nothing -> (Just x, False)
              Just s -> (Just s, True)

        r = case mleftmost of
          Nothing -> LayersHandlerRenderEntryDummy 0 <| newlentries''

          Just leftmostid -> r' where
            -- TODO you could probably do this more efficiently with a very bespoke fold but whatever
            (index, depth) = case Seq.findIndexL (\lentry -> _superOwl_id (_layerEntry_superOwl lentry) == leftmostid) lentries of
              Nothing -> error $ "expected to find id " <> show leftmostid <> " in " <> show lentries
              Just x -> (skipped, depth') where
                depth' = layerEntry_depth (Seq.index lentries x) + (if samelevel then 0 else 1)
                noskiplentries = Seq.drop (x+1) $ lentries
                skippedlentries = Seq.takeWhileL (\lentry -> layerEntry_depth lentry > depth') $ noskiplentries
                skipped = if samelevel then x + 1 + Seq.length skippedlentries else x+1

            r' = Seq.insertAt index (LayersHandlerRenderEntryDummy depth) newlentries''

    -- finally add the dots indicating drop spot depth
    mapaccumrfn_fordots mdropdepth lhre = case mdropdepth of
      Nothing -> case lhre of
        LayersHandlerRenderEntryDummy d -> (Just d, lhre)
        _ -> (mdropdepth, lhre)
      Just x -> case lhre of
        LayersHandlerRenderEntryNormal s _ lentry -> if layerEntry_depth lentry > x
          then (mdropdepth, LayersHandlerRenderEntryNormal s (Just x) lentry)
          else (Nothing, lhre)
        _ -> error "unexpected LayersHandlerRenderEntryDummy"

    (_, newlentries) = mapAccumR mapaccumrfn_fordots Nothing newlentries'


    layersHandlerRenderEntry_selected :: LayersHandlerRenderEntry -> Bool
    layersHandlerRenderEntry_selected (LayersHandlerRenderEntryNormal LHRESS_Selected _ _) = True
    layersHandlerRenderEntry_selected (LayersHandlerRenderEntryNormal LHRESS_InheritSelected _ _) = True
    layersHandlerRenderEntry_selected _ = False

    -- TODO test + hookup correctly
    mapaccumrfn_forchildselected (selstack, lastdepth) lhre = ((newstack, depth), newlhre) where
      selected = layersHandlerRenderEntry_selected lhre
      depth = layersHandlerRenderEntry_depth lhre
      (childSelected, newstack) = if depth > lastdepth 
        then (False, selected:selstack)
        else if selected 
          then case selstack of 
            [] -> error "expected something on selstack"
            x:xs -> (False, True:xs)
          else if depth < lastdepth
            then case selstack of
              [] -> error "this should never happen"
              x1:xs1 -> case xs1 of
                [] -> (x1, [x1])
                x2:xs2 -> (x1 && not x2, (x1 || x2) : xs2)
            else (False, selstack)
      newlhre = case lhre of 
        LayersHandlerRenderEntryNormal _ dots lentry -> LayersHandlerRenderEntryNormal LHRESS_ChildSelected dots lentry
        x -> x
    finallentries = mapAccumR mapaccumrfn_forchildselected ([], 0) newlentries

      


    -- TODO iterate backwards to determine parents with selected children
    -- acc0 = ([False],0)  (stack, lastdepth)
    -- if depth > lastdepth
      -- (push False stack, depth)
    -- if selected, (set top of stack to true, depth)
    -- if depth < lastdepth
      -- (pop stack and set top of stack to True, depth)
      -- mark as LHRESS_ChildSelected
    -- otherwise no change
    -- 




-- TODO FINISH WORK IN PROGRESS
data LayersRenameHandler = LayersRenameHandler {
    _layersRenameHandler_original :: LayersHandler
    , _layersRenameHandler_renaming   :: SuperOwl
    , _layersRenameHandler_index :: Int -- LayerEntries index of what we are renaming
    , _layersRenameHandler_zipper   :: TZ.TextZipper
  }

-- TODO finish
isValidLayerRenameChar :: Char -> Bool
isValidLayerRenameChar c = isAlphaNum c -- `and` oneof c "-_."

renameTextZipperTransform :: KeyboardKey -> Maybe (TZ.TextZipper -> TZ.TextZipper)
renameTextZipperTransform = \case
  KeyboardKey_Space -> Just $ TZ.insertChar ' '
  KeyboardKey_Char k | isValidLayerRenameChar k -> Just $ TZ.insertChar k
  KeyboardKey_Backspace             -> Just $ TZ.deleteLeft
  KeyboardKey_Delete                 -> Just $ TZ.deleteRight
  KeyboardKey_Left               -> Just $ TZ.left
  KeyboardKey_Right             -> Just $ TZ.right
  KeyboardKey_Home              -> Just $ TZ.home
  KeyboardKey_End                  -> Just $ TZ.end
  KeyboardKey_Paste t | T.all isValidLayerRenameChar t -> Just $ TZ.insert t
  _                                   -> Nothing

renameToAndReturn :: LayersRenameHandler -> Text -> PotatoHandlerOutput
renameToAndReturn lh@LayersRenameHandler {..} newName = r where
  controller = CTagRename :=> (Identity $ CRename {
      _cRename_deltaLabel = (isOwl_name _layersRenameHandler_renaming, newName)
    })
  r = def {
      _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler _layersRenameHandler_original
      , _potatoHandlerOutput_pFEvent = Just $ WSEManipulate (False, IM.fromList [(_superOwl_id _layersRenameHandler_renaming,controller)])
    }

toDisplayLines :: LayersRenameHandler -> TZ.DisplayLines ()
toDisplayLines LayersRenameHandler {..} = TZ.displayLinesWithAlignment TZ.TextAlignment_Left 1000 () () _layersRenameHandler_zipper

-- TODO confirm/cancel if click off the one we are renaming, cancle handler and pass input onto the replacement (see TODO in GoatWidget)
instance PotatoHandler LayersRenameHandler where
  pHandlerName _ = handlerName_layersRename

  -- we incorrectly reuse RelMouseDrag for LayersHandler even though LayersHandler doesn't care about canvas pan coords
  -- pan offset should always be set to 0 in RelMouseDrag
  pHandleMouse lh@LayersRenameHandler {..} phi@PotatoHandlerInput {..} rmd@(RelMouseDrag MouseDrag {..}) = let

    ls@(LayersState _ lentries scrollPos) = _potatoHandlerInput_layersState
    pfs = _potatoHandlerInput_pFState
    owltree = (_owlPFState_owlTree pfs)
    rawleposxy@(V2 rawxoffset rawlepos) = _mouseDrag_to
    leposxy@(V2 _ lepos) = V2 rawxoffset (rawlepos + scrollPos)

    renaminglepos = _layersRenameHandler_index

    in case _mouseDrag_state of
      MouseDragState_Down | lepos == renaminglepos -> r where
        xpos = case clickLayerNew lentries leposxy of
          Nothing -> error "this should never happen"
          Just (downsowl, _, xoff) -> xoff - 6

        dl = toDisplayLines lh
        nexttz = TZ.goToDisplayLinePosition xpos 0 dl _layersRenameHandler_zipper

        r = Just $ def {
            _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler lh {
                _layersRenameHandler_zipper = nexttz
              }
          }
      -- TODO drag + select when it's implemented in TZ
      MouseDragState_Dragging -> Just $ setHandlerOnly lh
      MouseDragState_Up -> Just $ setHandlerOnly lh

      -- TODO somehow add return args from
      -- renameToAndReturn lh (TZ.value _layersRenameHandler_zipper)
      -- in here as well
      _ -> pHandleMouse _layersRenameHandler_original phi rmd

  pHandleKeyboard lh@LayersRenameHandler {..} phi@PotatoHandlerInput {..} kbd = case kbd of
    -- don't allow ctrl shortcuts while renaming
    KeyboardData _ [KeyModifier_Ctrl] -> Just $ setHandlerOnly lh
    KeyboardData KeyboardKey_Return [] -> Just $ renameToAndReturn lh (TZ.value _layersRenameHandler_zipper)
    KeyboardData KeyboardKey_Esc [] ->    Just $ setHandlerOnly _layersRenameHandler_original
    KeyboardData (KeyboardKey_Scroll scroll) _ -> Just $ handleScroll lh phi scroll
    KeyboardData key [] ->  case renameTextZipperTransform key of
      Nothing -> Nothing
      Just f -> r where
        nexttz = f _layersRenameHandler_zipper
        r = Just $ def {
            _potatoHandlerOutput_nextHandler = Just $ SomePotatoHandler lh {
                _layersRenameHandler_zipper = nexttz
              }
          }
    _ -> Nothing

  -- TODO render renaming stuff (or do we do this in pRenderLayersHandler?)
  --pRenderHandler lh@LayersRenameHandler {..} PotatoHandlerInput {..} = emptyHandlerRenderOutput

  pIsHandlerActive LayersRenameHandler {..} = False

  pRenderLayersHandler lh@LayersRenameHandler {..} phi@PotatoHandlerInput {..} = r where
    r' = pRenderLayersHandler _layersRenameHandler_original phi
    entries' = _layersViewHandlerRenderOutput_entries r'

    adjustfn (LayersHandlerRenderEntryNormal lhress dots lentry) = LayersHandlerRenderEntryNormal newlhress dots lentry where
      newlhress = LHRESS_Renaming _layersRenameHandler_zipper
    adjustfn (LayersHandlerRenderEntryDummy _) = error "this should never happen"
    entries = Seq.adjust'  adjustfn _layersRenameHandler_index entries'
    r = LayersViewHandlerRenderOutput { _layersViewHandlerRenderOutput_entries = entries }
