{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Controller.Manipulator.ShapeSpec (
  spec
) where

import           Relude                                         hiding (empty,
                                                                 fromList)

import           Test.Hspec

import           Potato.Flow.GoatTester

import           Potato.Flow
import           Potato.Flow.Controller.Manipulator.TestHelpers

  
drawShape :: (Int, Int, Int, Int) -> GoatTester ()
drawShape (x, y, sx, sy) = do
  count <- getOwlCount
  setTool Tool_Shape
  tool <- getTool
  verifyEqual "tool is shape" tool Tool_Shape
  canvasMouseDown (x, y)
  canvasMouseDown (x+sx, y+sy)
  verifyOwlCount (count+1)
  canvasMouseUp (x+sx, y+sy)
  let
    f sowl = case _superOwl_elt sowl of
      OwlItem _ (OwlSubItemEllipse _) -> Nothing
      xx                           -> Just ("expected ellipse, got " <> show xx)
  verifySelectionIsAndOnlyIs "shape is selected" f

basic_test :: Spec
basic_test = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do
  drawShape (0, 0, 100, 100)

basic_cancel :: Spec
basic_cancel = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do
  drawShape (0, 0, 100, 100)
  canvasMouseDown (0, 0)
  canvasMouseUp (0, 0)
  count <- getOwlCount
  setTool Tool_Shape
  canvasMouseDown (0, 0)
  canvasMouseDown (10, 10)
  verifyOwlCount (count+1)
  pressEscape
  verifyOwlCount count

spec :: Spec
spec = do
  describe "Shape" $ do
    describe "basic" $ basic_test
    describe "basic_cancel" $ basic_cancel
