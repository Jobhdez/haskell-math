module Vector where

import MatrixVectorClass

instance MatrixVecMathObject Vector where
  add = computeVectors (+)
  sub = computeVectors (-)
  mul = computeVectors (*)
  pow = powv
  exponential = exponentialv
  logarithm = vecLog
  absolute = vecAbs
  maxim = maximumv
  minim = minimumv


data Vector = Vector [Int] deriving (Show, Eq)

computeVectors :: (Int -> Int -> Int) -> (Vector -> Vector -> Vector)
computeVectors fn (Vector v1) (Vector v2) =
  Vector $ zipWith fn v1 v2


powv :: Vector -> Int -> Vector
powv (Vector v1) int =
  (Vector v2)
  where
    v2 = map (\x -> x^int) v1
  
exponentialv :: Vector -> MatVecf
exponentialv (Vector v1) =
  (Vf v2)
  where
    v2 = map (\x -> exp (fromIntegral x)) v1


vecLog :: Vector -> MatVecf
vecLog (Vector v1) =
  (Vf v2)
  where
    v2 = map (\x -> Prelude.log (fromIntegral x)) v1


dotP :: Vector -> Vector -> Int
dotP v1 v2 =
  sumElements v3
  where
    v3 = mul v1 v2


sumElements :: Vector -> Int
sumElements (Vector v1) =
  sum v1

vecAbs :: Vector -> Vector
vecAbs (Vector v) =
  (Vector v2) where
  v2 = map (\x -> Prelude.abs x) v

maximumv :: Vector -> Vector -> Vector
maximumv (Vector v1) (Vector v2) =
  (Vector v3) where
  v3 = map (\(x,y) -> if x<y then y else x) $ zip v1 v2

minimumv :: Vector -> Vector -> Vector
minimumv (Vector v1) (Vector v2) =
  (Vector v3) where
  v3 = map (\(x,y) -> if x<y then x else y) $ zip v1 v2
