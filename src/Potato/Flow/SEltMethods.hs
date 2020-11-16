{-# LANGUAGE RecordWildCards #-}

module Potato.Flow.SEltMethods (
  getSEltBox
  , updateFnFromController
  , RenderFn
  , SEltDrawer(..)
  , getDrawer
) where

import           Relude

import           Potato.Flow.Math
import           Potato.Flow.SElts
import           Potato.Flow.Types

import           Data.Dependent.Sum (DSum ((:=>)), (==>))
import           Data.Maybe         (fromJust)

-- TODO rename to getSEltBoundingBox or something
-- | gets an 'LBox' that contains the entire RElt
getSEltBox :: SElt -> Maybe LBox
getSEltBox selt = case selt of
  SEltNone        -> Nothing
  SEltFolderStart -> Nothing
  SEltFolderEnd   -> Nothing
  -- TODO return canonical
  SEltBox x       -> Just $ canonicalLBox_from_lBox_ $ _sBox_box x
  SEltLine x      -> Just $ make_LBox_from_XYs (_sLine_start x) (_sLine_end x)
  SEltText x      -> Just $ canonicalLBox_from_lBox_ $ _sText_box x

type RenderFn = XY -> Maybe PChar

makePotatoRenderer :: LBox -> RenderFn
makePotatoRenderer lbox pt = if does_LBox_contains_XY lbox pt
  then Just '#'
  else Nothing

data SEltDrawer = SEltDrawer {
  _sEltDrawer_box        :: LBox
  , _sEltDrawer_renderFn :: RenderFn -- switch to [RenderFn] for better performance
}

nilDrawer :: SEltDrawer
nilDrawer = SEltDrawer {
    _sEltDrawer_box = nilLBox
    , _sEltDrawer_renderFn = const Nothing
  }

getDrawer :: SElt -> SEltDrawer
getDrawer selt = case selt of
  SEltNone        -> nilDrawer
  SEltFolderStart -> nilDrawer
  SEltFolderEnd   -> nilDrawer
  SEltBox SBox {..}       -> r where
    CanonicalLBox _ _ lbox@(LBox (V2 x y) (V2 w h)) = canonicalLBox_from_lBox _sBox_box
    rfn pt@(V2 x' y')
      | not (does_LBox_contains_XY lbox pt) = Nothing
      | w == 1 && h == 1 = Just $ _superStyle_point _sBox_style
      | w == 1 = Just $ _superStyle_vertical _sBox_style
      | h == 1 = Just $ _superStyle_horizontal _sBox_style
      | x' == x && y' == y = Just $ _superStyle_tl _sBox_style
      | x' == x && y' == y+h-1 = Just $ _superStyle_bl _sBox_style
      | x' == x+w-1 && y' == y = Just $ _superStyle_tr _sBox_style
      | x' == x+w-1 && y' == y+h-1 = Just $ _superStyle_br _sBox_style
      | x' == x || x' == x+w-1 = Just $ _superStyle_vertical _sBox_style
      | y' == y || y' == y+h-1 = Just $ _superStyle_horizontal _sBox_style
      | otherwise = case _superStyle_fill _sBox_style of
        FillStyle_Simple c -> Just c
        FillStyle_Blank    -> Nothing
    r = SEltDrawer {
        _sEltDrawer_box = lbox
        , _sEltDrawer_renderFn = rfn
      }
  SEltLine _      -> potatoDrawer
  SEltText _      -> potatoDrawer
  where
    potatoDrawer = SEltDrawer {
        _sEltDrawer_box = fromJust (getSEltBox selt)
        , _sEltDrawer_renderFn =  makePotatoRenderer $ fromJust (getSEltBox selt)
      }

modify_sElt_with_cBoundingBox :: Bool -> SElt -> CBoundingBox -> SElt
modify_sElt_with_cBoundingBox isDo selt CBoundingBox {..} = case selt of
  SEltBox SBox {..}  -> SEltBox $ SBox {
      _sBox_box = modifyDelta isDo _sBox_box _cBoundingBox_deltaBox
      , _sBox_style = _sBox_style
    }
  -- TODO handle resize parameter
  SEltLine SLine {..} -> SEltLine $ SLine {
      _sLine_start = modifyDelta isDo _sLine_start
        (_deltaLBox_translate _cBoundingBox_deltaBox)
      , _sLine_end = modifyDelta isDo _sLine_end
        (_deltaLBox_translate _cBoundingBox_deltaBox)
      , _sLine_style = _sLine_style
    }
  SEltText SText {..} -> SEltText $ SText {
      _sText_box     = modifyDelta isDo _sText_box _cBoundingBox_deltaBox
      , _sText_text  = _sText_text
      , _sText_style = _sText_style
    }
  x          -> x

modifyDelta :: (Delta x dx) => Bool -> x ->  dx -> x
modifyDelta isDo x dx = if isDo
  then plusDelta x dx
  else minusDelta x dx

updateFnFromController :: Bool -> Controller -> (SEltLabel -> SEltLabel)
updateFnFromController isDo = \case
  (CTagRename :=> Identity d) -> \seltl -> modifyDelta isDo seltl d
  (CTagBox :=> Identity d) -> \(SEltLabel sname selt) -> case selt of
    SEltBox s -> SEltLabel sname (SEltBox $ modifyDelta isDo s d)
    _ -> error $ "Controller - SElt type mismatch: CTagBox - " <> show selt
  (CTagLine :=> Identity d) -> \(SEltLabel sname selt) -> case selt of
    SEltLine s -> SEltLabel sname (SEltLine $ modifyDelta isDo s d)
    _ -> error $ "Controller - SElt type mismatch: CTagLine - " <> show selt
  (CTagText :=> Identity d) -> \(SEltLabel sname selt) -> case selt of
    SEltText s -> SEltLabel sname (SEltText $ modifyDelta isDo s d)
    _ -> error $ "Controller - SElt type mismatch: CTagText - " <> show selt
  (CTagBoundingBox :=> Identity d) -> \(SEltLabel sname selt) ->
    SEltLabel sname (modify_sElt_with_cBoundingBox isDo selt d)
