module MatrixVec where


data Matrix = Matrix [[Int]] deriving (Show, Eq)
data Vector = Vector [Int] deriving (Show, Eq)

addMatrixVec :: [[Int]] -> [Int] -> [[Int]]
addMatrixVec [] vec = []
addMatrixVec (x:xs) vec =
  addVecs x vec:rest
  where
    rest = addMatrixVec mv vec where
      mv = xs


subMatrixVec :: [[Int]] -> [Int] -> [[Int]]
subMatrixVec [] vec = []
subMatrixVec (x:xs) vec =
  subVecs x vec:rest
  where
    rest = subMatrixVec mv vec where
      mv = xs

mulMatrixVec ::  [[Int]] -> [Int] -> [Int]
mulMatrixVec [] vec = []
mulMatrixVec (x:xs) vec =
  sumProducts x vec:rest
  where
    rest = mulMatrixVec xs vec


sumProducts :: [Int] -> [Int] -> Int
sumProducts v v2 =
  sum v3
  where
    v3 = multiply v v2

multiply :: [Int] -> [Int] -> [Int]
multiply [] v2 = []
multiply (x:xs) (y:ys) =
  x*y:rest
  where
    rest = multiply xs ys


addVecs :: [Int] -> [Int] -> [Int]
addVecs [] v2 = []
addVecs (x:xs) (y:ys) =
  x + y:rest
  where
    rest = addVecs xs ys

subVecs :: [Int] -> [Int] -> [Int]
subVecs [] v2 = []
subVecs (x:xs) (y:ys) =
  x - y:rest
  where
    rest = subVecs xs ys
  
