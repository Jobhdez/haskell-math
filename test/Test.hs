module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "test the determinant of the matrix" $ do
      3 + 3 `shouldBe` 6
      
