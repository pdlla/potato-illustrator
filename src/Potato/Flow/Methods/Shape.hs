

module Potato.Flow.Methods.Shape where

import          Relude

import           Potato.Flow.Math
import           Potato.Flow.Methods.Types
import Potato.Flow.Attachments
import           Potato.Flow.Serialization.Snake
import          Potato.Flow.Llama
import Potato.Flow.Controller.Types
import Potato.Flow.OwlItem


data ShapeDef o = ShapeDef {
  _shapeDef_name :: Text
  , _shapeDef_create :: PotatoDefaultParameters -> LBox -> OwlItem
  , _shapeDef_impl :: o -> ShapeImpl
}

data ShapeImpl = ShapeImpl {
  _shapeImpl_updateFromLBox :: REltId -> LBox -> Llama
  , _shapeImpl_toLBox :: LBox
  , _shapeImpl_textArea :: Maybe CanonicalLBox
  , _shapeImpl_textLabel :: [CanonicalLBox]
  , _shapeImpl_startingAttachments :: [AvailableAttachment]
  --TODO this should take a OwlItemCache?
  , _shapeImpl_draw :: SEltDrawer
}

