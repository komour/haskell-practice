module AllTests where

import           Task1

import           Test.Tasty
import           Test.Tasty.Hspec

allTestsTree :: IO TestTree
allTestsTree = testSpec "unit-tests" allTests

allTests :: Spec
allTests =
  describe "Task1" $ do
    it "perimeter" $ do
      perimeter [Point 0 0, Point 1 0, Point 1 1, Point 0 1] `shouldBe` 4
      perimeter [Point 0 0, Point (-2) 0, Point (-2) (-2), Point 0 (-2)] `shouldBe` 8
    it "doubleArea" $ do
      doubleArea [Point 0 0, Point 1 0, Point 1 1, Point 0 1] `shouldBe` 2
      doubleArea [Point 0 0, Point (-2) 0, Point (-2) (-2), Point 0 (-2)] `shouldBe` 8
