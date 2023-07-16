module Vector where

import ArithTypeClass

instance ArithMathObject Vector where
  add = computeVectors (+)
  sub = computeVectors (-)
  mul = computeVectors (*)


data Vector = Vector [Int]

computeVectors :: (Int -> Int -> Int) -> (Vector -> Vector -> Vector)
computeVectors fn (Vector v1) (Vector v2) =
  Vector $ zipWith fn v1 v2



