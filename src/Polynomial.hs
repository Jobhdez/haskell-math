module Polynomial where

import ArithTypeClass

instance ArithMathObject Polynomial where
  add = addPolynomials (+)
  sub = subPolynomials (-)
  mul = mulPolynomials (*)


data Polynomial = Polynomial [Int] deriving (Show, Eq)

addPolynomials :: (Int -> Int -> Int) -> (Polynomial -> Polynomial -> Polynomial)
addPolynomials fn (Polynomial p) (Polynomial p2) =
  Polynomial $ zipWith fn p p2

subPolynomials :: (Int -> Int -> Int) -> (Polynomial -> Polynomial -> Polynomial)
subPolynomials fn (Polynomial p) (Polynomial p2) =
  Polynomial $ zipWith fn p p2


mulPolynomials :: (Int -> Int -> Int) -> (Polynomial -> Polynomial -> Polynomial)
mulPolynomials fn (Polynomial []) (Polynomial p2) = Polynomial []
mulPolynomials fn (Polynomial p1) (Polynomial p2)  =
  poly
  where
    poly = append poly2 rest where
      poly2 =  (Polynomial poly3) where
        poly3 = multiply fn (head p1) p2
      rest = mulPolynomials fn (Polynomial p3) (Polynomial p2) where
        p3 = tail p1
      
multiply :: (Int -> Int -> Int) -> Int -> [Int] -> [Int]
multiply fn coeff [] = []
multiply fn coeff (x:xs) =
  fn coeff x:rest
  where
    rest = multiply fn coeff xs

append :: Polynomial -> Polynomial -> Polynomial
append (Polynomial p) (Polynomial p2) =
  (Polynomial p3)
  where
    p3 = p ++ p2
