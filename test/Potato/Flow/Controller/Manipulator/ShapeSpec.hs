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

  
fetchLatestEllipse :: OwlPFState -> Either Text SEllipse
fetchLatestEllipse pfs = do
  sowl <- case maybeGetMostRecentlyCreatedOwl' pfs of
    Nothing -> Left "failed, no 🦉s"
    Just x  -> Right x
  case _owlItem_subItem (_superOwl_elt sowl) of
    OwlSubItemEllipse x -> Right x
    x                -> Left $ "expected SEllipse got: " <> show x

verifyMostRecentlyCreatedEllipseHasSize :: (Int, Int) -> GoatTester ()
verifyMostRecentlyCreatedEllipseHasSize (x, y) = verifyStateObjectHasProperty "verifyMostRecentlyCreatedEllipseHasSize" fetchLatestEllipse checkfn where 
  checkfn sellipse = r where
    LBox _ (V2 x' y') = _sEllipse_box sellipse
    r = if x == x' && y == y'
      then Nothing
      else Just $ "got size " <> show (x', y') <> " expected " <> show (x, y)


drawShape :: (Int, Int, Int, Int) -> GoatTester ()
drawShape (x, y, sx, sy) = do
  count <- getOwlCount
  setTool Tool_Shape
  tool <- getTool
  verifyEqual "tool is shape" tool Tool_Shape

  -- TODO set shape, it's hardcoded right now

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

modifyEllipse_test :: Spec
modifyEllipse_test = hSpecGoatTesterWithOwlPFState emptyOwlPFState $ do
  drawShape (1, 1, 100, 100)
  verifyMostRecentlyCreatedEllipseHasSize (100,100)
  canvasMouseDown (0, 0)
  canvasMouseDown (-10, -10)
  canvasMouseUp (-10, -10)
  verifyMostRecentlyCreatedEllipseHasSize (110,110)

spec :: Spec
spec = do
  describe "Shape" $ do
    describe "basic" $ basic_test
    describe "basic_cancel" $ basic_cancel
    describe "modifyEllipse_test" $ modifyEllipse_test
