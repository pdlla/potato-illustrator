

module Potato.Flow.Methods.Shape where

import          Relude

import           Potato.Flow.Math
import           Potato.Flow.Methods.Types
import Potato.Flow.Attachments
import           Potato.Flow.Serialization.Snake




data ShapeImpl o = ShapeImpl {
  _shapeImpl_name :: Text
  , _shapeImpl_updateFromLBox :: o -> LBox -> o
  , _shapeImpl_toLBox :: o -> LBox
  , _shapeImpl_textArea :: o -> Maybe CanonicalLBox
  , _shapeImpl_textLabel :: o -> [CanonicalLBox]
  , _shapeImpl_startingAttachments :: o -> [AvailableAttachment]
  --TODO this should take a OwlItemCache?
  , _shapeImpl_draw :: o -> SEltDrawer

}


{-

boxShapeImpl :: ShapeImpl SBox
boxShapeImpl = ShapeImpl {
    _shapeImpl_name = "SBox"

    , _shapeImpl_updateFromLBox = \sbox lbox -> undefined
    , _shapeImpl_toLBox = \sbox -> _sBox_box sbox
    , _shapeImpl_textArea = \sbox -> if sBoxType_isText (_sBox_boxType sbox) 
      then Just (getSBoxTextBox sbox)
      else Nothing 
    , _shapeImpl_textLabel = \sbox  -> if sBoxType_hasBorder (_sBox_boxType sbox) 
      then [canonicalLBox_from_lBox (lBox_to_boxLabelBox (_sBox_box sbox))]
      else []
    , _shapeImpl_startingAttachments = \sbox -> if sBoxType_hasBorder (_sBox_boxType sbox)
      then []
      else availableAttachLocationsFromLBox True (_sBox_box sbox)
    , _shapeImpl_draw = \sbox -> sBox_drawer sbox
  }
-}