module LinearAlgebra where

determinant :: [[Int]] -> Int
determinant matrix =
  e1 - e2 + e3
  where
    row = matrix!!0
    e1 = row!!0 * computeTwoDDeterminant(removeColumn matrix 0)
    e2 = row!!1 * computeTwoDDeterminant(removeColumn matrix  1)
    e3 = row!!2 * computeTwoDDeterminant(removeColumn matrix 2)
      
removeColumn :: [[Int]] -> Int -> [[Int]]
removeColumn matrix column =
  let matrix2 = drop 1 matrix in
    map (\x -> take column x ++ drop (1 + column) x) matrix2

computeTwoDDeterminant :: [[Int]] -> Int
computeTwoDDeterminant matrix =
  e - e2
  where
    row = matrix!!0
    row2 = matrix!!1
    e = row!!0 * row2!!1
    e2 = row!!1 * row2!!0

trace :: [[Int]] -> Int
trace matrix =
  -- expects a square matrix
  -- Example:
  --   ghci> trace [[2,1,5],[2,3,4],[0,1,0]]
  --   5
  sum (traceHelper matrix 0)
  
traceHelper :: [[Int]] -> Int -> [Int]
traceHelper [] n = []
traceHelper (x:xs) n =
  (x!!n):rest
  where
    rest = traceHelper xs (n+1)

upperTriangular :: [[Int]] -> [[Int]]
upperTriangular sqMatrix =
  upperTriangularHelper sqMatrix 0

lowerTriangular :: [[Int]] -> [[Int]]
lowerTriangular sqMatrix =
  let row = sqMatrix!!0 in
    let numZeros = (length row) - 1 in
      lowerTriangularHelper sqMatrix numZeros
    
upperTriangularHelper :: [[Int]] -> Int -> [[Int]]
upperTriangularHelper [] n = []
upperTriangularHelper (x:xs) numOfZeros =
  insertZeros x numOfZeros:rest
  where
    rest = upperTriangularHelper xs (numOfZeros + 1)

lowerTriangularHelper :: [[Int]] -> Int -> [[Int]]
lowerTriangularHelper [] zeros = []
lowerTriangularHelper (x:xs) zeros =
  lowerInsertZeros x zeros:rest
  where
    rest = lowerTriangularHelper xs (zeros - 1)
    
lowerInsertZeros :: [Int] -> Int -> [Int]
lowerInsertZeros row zeros =
  let numZeros = insertZerosHelper zeros in
    let rowLength = length row in
      take (rowLength - zeros) row ++ numZeros

insertZeros :: [Int] -> Int -> [Int]
insertZeros row numOfZeros =
  let zeros = insertZerosHelper numOfZeros in
   zeros ++ drop numOfZeros row
   
insertZerosHelper :: Int  -> [Int]
insertZerosHelper 0 = []
insertZerosHelper numOfZeros =
  0:rest
  where
    rest = insertZerosHelper (numOfZeros - 1)
  
