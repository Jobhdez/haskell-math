module MatrixVectorClass where

data MatVecf = Mf [[Float]] | Vf [Float] deriving (Show, Eq)

class MatrixVecMathObject a where
  add :: a -> a -> a
  sub :: a -> a -> a
  mul :: a -> a -> a
  pow :: a -> Int -> a
  exponential :: a -> MatVecf
  logarithm :: a -> MatVecf
  absolute :: a -> a
  maxim :: a -> a -> a
  minim :: a -> a -> a


  



