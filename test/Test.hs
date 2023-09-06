module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import LinearAlgebra


main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "test the determinant of the matrix" $ do
      determinant [[1,2,3], [45,60,33], [100,45,32]] `shouldBe` -7770

    it "test the trace of a square matrix" $ do
      trace [[2,1,5],[2,3,4],[0,1,0]] `shouldBe` 5
      
      
