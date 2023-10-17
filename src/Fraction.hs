module Fraction where

data  Fraction = Fraction {
  numerator :: Int,
  denominator :: Int
  } deriving (Show, Eq)

addFractions :: Fraction -> Fraction -> Fraction
addFractions fraction1 fraction2 =
  (Fraction n d)
  where
    n =  ((numerator fraction1) * (denominator fraction2)) + ((numerator fraction2) * (denominator fraction1))
    d = (denominator fraction1) * (denominator fraction2)


subFractions :: Fraction -> Fraction -> Fraction
subFractions (Fraction num denom) (Fraction num2 denom2) =
  (Fraction n d)
  where
    n = (num * denom2) - (num2 * denom)
    d = denom*denom2

mulFractions :: Fraction -> Fraction -> Fraction
mulFractions (Fraction num denom) (Fraction num2 denom2) =
  (Fraction n d)
  where
    n = num *  num2
    d = denom * denom2


   
    
