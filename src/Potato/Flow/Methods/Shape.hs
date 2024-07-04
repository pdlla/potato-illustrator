

module Potato.Flow.Methods.Shape where

import          Relude

import           Potato.Flow.Math
import           Potato.Flow.Methods.Types
import           Potato.Flow.Methods.SEltMethods
import Potato.Flow.Attachments
import           Potato.Flow.Serialization.Snake
import          Potato.Flow.Llama
import Potato.Flow.Controller.Types
import Potato.Flow.OwlItem
import Potato.Flow.Owl
import Potato.Flow.Controller.Manipulator.TextInputState

import Data.Default
import qualified Text.Show


-- TODO probably just combine this with ShapeImpl and make ShapeModifyHandler take an `o` parameter
data ShapeDef o = ShapeDef {
  _shapeDef_name :: Text
  , _shapeDef_create :: PotatoDefaultParameters -> LBox -> OwlItem
  , _shapeDef_impl :: o -> ShapeImpl
  -- NOTE these are separate from _shapeDef_impl as I didn't want to rewrite TextImpl to pull out the `o` 
  , _shapeDef_labelImpl :: Int -> TextImpl o
  , _shapeDef_textImpl :: TextImpl o
}






data ShapeImpl = ShapeImpl {
  _shapeImpl_updateFromLBox :: REltId -> LBox -> Llama
  , _shapeImpl_toLBox :: LBox
  , _shapeImpl_textArea :: Maybe CanonicalLBox
  -- TODO rename to _shapeImpl_textLabels
  , _shapeImpl_textLabels :: [CanonicalLBox]
  , _shapeImpl_startingAttachments :: [AvailableAttachment]
  --TODO this should take a OwlItemCache?
  , _shapeImpl_draw :: SEltDrawer
}

instance Show ShapeImpl where
  show _ = "ShapeImpl"

emptyShapeDef :: ShapeDef ()
emptyShapeDef = ShapeDef {
  _shapeDef_name = "empty"
  , _shapeDef_create = \_ _ -> error "emptyShapeDef"
  , _shapeDef_impl = \_ -> emptyShapeImpl
  , _shapeDef_labelImpl = \_ -> error "emptyShapeDef"
  , _shapeDef_textImpl = error "emptyShapeDef"
}

emptyShapeImpl :: ShapeImpl
emptyShapeImpl = ShapeImpl {
  _shapeImpl_updateFromLBox = error "emptyShapeImpl"
  , _shapeImpl_toLBox = LBox 0 0
  , _shapeImpl_textArea = Nothing
  , _shapeImpl_textLabels = []
  , _shapeImpl_startingAttachments = []
  , _shapeImpl_draw = error "emptyShapeImpl"
}


ellipseShapeDef :: ShapeDef SEllipse
ellipseShapeDef = ShapeDef {
  _shapeDef_name = "ellipse"
  , _shapeDef_create = \_ lbox -> OwlItem (OwlInfo "<ellipse>") (OwlSubItemEllipse (def {_sEllipse_box = lbox}))
  , _shapeDef_impl = \sellipse -> makeEllipseShapeImpl sellipse
}

makeEllipseShapeImpl :: SEllipse -> ShapeImpl
makeEllipseShapeImpl sellipse = ShapeImpl {
  _shapeImpl_updateFromLBox = \rid lbox -> makeSetLlama (rid, SEltEllipse $ sellipse {_sEllipse_box = lbox})
  , _shapeImpl_toLBox = _sEllipse_box sellipse
  -- TODO be smarter about roundoff
  , _shapeImpl_textArea = let 
      LBox (V2 x y) (V2 w h) = _sEllipse_box sellipse
      -- fix this for me
      neww = floor (0.7 * fromIntegral w)
      newh = floor (0.7 * fromIntegral h)
    in
      Just $ canonicalLBox_from_lBox $ LBox (V2 (x + (w - neww) `div` 2) (y + (h - newh) `div` 2)) (V2 neww newh)
  , _shapeImpl_textLabels = []
  , _shapeImpl_startingAttachments = []
  , _shapeImpl_draw = sEllipse_drawer sellipse
}
