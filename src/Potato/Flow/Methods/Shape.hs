

module Potato.Flow.Methods.Shape where

import          Relude

import           Potato.Flow.Math
import           Potato.Flow.Methods.Types
import Potato.Flow.Attachments
import           Potato.Flow.Serialization.Snake
import          Potato.Flow.Llama
import Potato.Flow.Controller.Types
import Potato.Flow.OwlItem

import qualified Text.Show

data ShapeDef o = ShapeDef {
  _shapeDef_name :: Text
  , _shapeDef_create :: PotatoDefaultParameters -> LBox -> OwlItem
  , _shapeDef_impl :: o -> ShapeImpl
}

data ShapeImpl = ShapeImpl {
  _shapeImpl_updateFromLBox :: REltId -> LBox -> Llama
  , _shapeImpl_toLBox :: LBox
  , _shapeImpl_textArea :: Maybe CanonicalLBox
  -- TODO rename to _shapeImpl_textLabels
  , _shapeImpl_textLabel :: [CanonicalLBox]
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
}

emptyShapeImpl :: ShapeImpl
emptyShapeImpl = ShapeImpl {
  _shapeImpl_updateFromLBox = error "emptyShapeImpl"
  , _shapeImpl_toLBox = LBox 0 0
  , _shapeImpl_textArea = Nothing
  , _shapeImpl_textLabel = []
  , _shapeImpl_startingAttachments = []
  , _shapeImpl_draw = error "emptyShapeImpl"
}

