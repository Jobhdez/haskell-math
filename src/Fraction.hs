module Fraction where

import ArithTypeClass

instance ArithMathObject Fraction where
  add = addFractions (+)
  sub = subFractions (-)
  mul = mulFractions (*)

data  Fraction = Fraction (Int, Int) deriving (Show, Eq)

addFractions :: (Int -> Int -> Int) -> (Fraction -> Fraction -> Fraction)
addFractions fn (Fraction (num, denom)) (Fraction (num2, denom2)) =
  (Fraction (n, d))
  where
    n = fn (num * denom2) (num2 * denom)
    d = denom*denom2


subFractions :: (Int -> Int -> Int) -> (Fraction -> Fraction -> Fraction)
subFractions fn (Fraction (num, denom)) (Fraction (num2, denom2)) =
  (Fraction (n, d))
  where
    n = fn (num * denom2) (num2 * denom)
    d = denom*denom2

mulFractions :: (Int -> Int -> Int) -> (Fraction -> Fraction -> Fraction)
mulFractions fn (Fraction (num, denom)) (Fraction (num2, denom2)) =
  (Fraction (n, d))
  where
    n = fn num num2
    d = fn denom denom2


   
    
