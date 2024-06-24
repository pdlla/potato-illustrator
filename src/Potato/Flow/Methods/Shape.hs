

module Potato.Flow.Methods.Shape where

import          Relude

import           Potato.Flow.Math
import           Potato.Flow.Methods.Types
import Potato.Flow.Attachments
import           Potato.Flow.Serialization.Snake
import          Potato.Flow.Llama
import Potato.Flow.Controller.Types
import Potato.Flow.OwlItem



data ShapeImpl o = ShapeImpl {
  _shapeImpl_name :: Text
  , _shapeImpl_create :: PotatoDefaultParameters -> LBox -> OwlItem
  , _shapeImpl_updateFromLBox :: o -> LBox -> o
  , _shapeImpl_toLBox :: o -> LBox
  , _shapeImpl_textArea :: o -> Maybe CanonicalLBox
  , _shapeImpl_textLabel :: o -> [CanonicalLBox]
  , _shapeImpl_startingAttachments :: o -> [AvailableAttachment]
  --TODO this should take a OwlItemCache?
  , _shapeImpl_draw :: o -> SEltDrawer

}

