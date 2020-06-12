module Main where

import           AllTests   (allTestsTree)
import           Test.Tasty

main :: IO ()
main =
  allTestsTree >>= \unitTests ->
    let allTests = testGroup "unit-tests" [unitTests]
     in defaultMain allTests
