module Vector where

import ArithTypeClass

instance ArithMathObject Vector where
  add = computeVectors (+)
  sub = computeVectors (-)
  mul = computeVectors (*)


data Vector = Vector [Int] deriving (Show, Eq)

computeVectors :: (Int -> Int -> Int) -> (Vector -> Vector -> Vector)
computeVectors fn (Vector v1) (Vector v2) =
  Vector $ zipWith fn v1 v2


powv :: Vector -> Int -> Vector
powv (Vector v1) int =
  (Vector v2)
  where
    v2 = map (\x -> x^int) v1
  
exponentialv :: Vector -> [Float]
exponentialv (Vector v1) =
  map (\x -> exp (fromIntegral x)) v1


vecLog :: Vector -> [Float]
vecLog (Vector v1) =
  map (\x -> log (fromIntegral x)) v1
