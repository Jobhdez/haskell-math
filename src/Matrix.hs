module Matrix where

import ArithTypeClass
import Data.List (transpose)

instance ArithMathObject Matrix where
  add = computeMatrices (+)
  sub = computeMatrices (-)
  mul = mulMatrices (*)


data Matrix = Matrix [[Int]] deriving (Show, Eq)

computeMatrices :: (Int -> Int -> Int) -> (Matrix -> Matrix -> Matrix)
computeMatrices fn (Matrix m1) (Matrix m2) =
  Matrix $ compute m1 m2
  where
    compute = zipWith (zipWith fn)


mulMatrices :: (Int -> Int -> Int) -> (Matrix -> Matrix -> Matrix)
mulMatrices fn (Matrix m1) (Matrix m2) =
 let transposedMat = (Matrix (transpose m2)) in
   matmul (Matrix m1) transposedMat


matmul :: Matrix -> Matrix -> Matrix
matmul (Matrix []) _ = Matrix []
matmul (Matrix (x:xs)) (Matrix m2) =
  Matrix productMat
  where
    productMat = dot:rest
      where
        dot = dotProduct x (Matrix m2)
        Matrix rest = matmul (Matrix xs) (Matrix m2)

dotProduct :: [Int] -> Matrix -> [Int]
dotProduct vec (Matrix []) = []
dotProduct vec (Matrix (y:ys)) =
  sum (vecMul vec y):rest
  where
    rest = dotProduct vec (Matrix ys)

vecMul :: [Int] -> [Int] -> [Int]
vecMul [] v2 = []
vecMul (x:xs) (y:ys) =
  x*y:rest
  where
    rest = vecMul xs ys


pow :: Matrix -> Int -> Matrix
pow (Matrix m1) int =
  (Matrix m2)
  where
    m2 = map (\x -> map (\x -> x ^ int) x) m1

exponential :: Matrix -> [[Float]]
exponential (Matrix m1) =
  map (\x -> map (\x -> exp (fromIntegral x)) x) m1

matLog :: Matrix -> [[Float]]
matLog (Matrix m1) =
  map (\x -> map (\x -> log (fromIntegral x)) x) m1

matAbs :: Matrix -> Matrix
matAbs (Matrix m1) =
  (Matrix m2) where
  m2 = map (\x -> map (\x -> abs x) x) m1

maximumm :: Matrix -> Matrix -> Matrix
maximumm (Matrix m1) (Matrix m2) =
  (Matrix m3) where
  m3 = map (\(x,y) -> map (\(x,y) -> if x < y then y else x) $ zip x y) $ zip m1 m2

minimumm :: Matrix -> Matrix -> Matrix
minimumm (Matrix m1) (Matrix m2) =
  (Matrix m3) where
  m3 = map (\(x,y) -> map (\(x,y) -> if x < y then x else y) $ zip x y) $ zip m1 m2

  
