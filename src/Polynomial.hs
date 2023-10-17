module Polynomial where


data Polynomial = Polynomial [Int] deriving (Show, Eq)

addPolynomials :: Polynomial -> Polynomial -> Polynomial
addPolynomials (Polynomial p) (Polynomial p2) =
  Polynomial $ zipWith (+) p p2

subPolynomials :: Polynomial -> Polynomial -> Polynomial
subPolynomials (Polynomial p) (Polynomial p2) =
  Polynomial $ zipWith (-)  p p2

mulPolynomials :: Polynomial -> Polynomial -> Polynomial
mulPolynomials (Polynomial []) (Polynomial p2) = Polynomial []
mulPolynomials  (Polynomial p1) (Polynomial p2)  =
  poly
  where
    poly = append poly2 rest where
      poly2 =  (Polynomial poly3) where
        poly3 = multiply (head p1) p2
      rest = mulPolynomials (Polynomial p3) (Polynomial p2) where
        p3 = tail p1
      
multiply :: Int -> [Int] -> [Int]
multiply coeff [] = []
multiply coeff (x:xs) =
  (coeff *  x):rest
  where
    rest = multiply coeff xs

append :: Polynomial -> Polynomial -> Polynomial
append (Polynomial p) (Polynomial p2) =
  (Polynomial p3)
  where
    p3 = p ++ p2
