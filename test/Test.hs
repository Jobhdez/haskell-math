module Main where

import Test.Hspec
import LinearAlgebra
import ArithTypeClass
import Matrix
import Vector
import MatrixVec
import Polynomial
import Fraction
import NeuralNetworks

main :: IO ()
main = hspec $ do
  describe "Linear Algebra" $ do
    it "test the determinant of the matrix" $ do
      determinant [[1,2,3], [45,60,33], [100,45,32]] `shouldBe` -7770

    it "test the trace of a square matrix" $ do
      trace [[2,1,5],[2,3,4],[0,1,0]] `shouldBe` 5

    it "test the uppertriangular of a square matrix" $ do
      upperTriangular  [[1,2,3,4], [5,6,7,8], [9,10,11,12], [13,14,15,16]] `shouldBe` [[1,2,3,4],[0,6,7,8],[0,0,11,12],[0,0,0,16]]

    it "test the lowertriangular of a square matrix" $ do
      lowerTriangular [[1,2,3,4], [5,6,7,8], [9,10,11,12], [13,14,15,16]] `shouldBe` [[1,0,0,0],[5,6,0,0],[9,10,11,0],[13,14,15,16]]

  describe "Matrix Arith" $ do
    it "test matrix addition" $ do
      add (Matrix [[3,4,5],[5,6,7]])  (Matrix [[1,2,3],[1,1,1]]) `shouldBe` (Matrix [[4,6,8],[6,7,8]])
      
    it "test matrix subtraction" $ do
      sub (Matrix [[3,4,5],[5,6,7]])  (Matrix [[1,2,3],[1,1,1]]) `shouldBe` (Matrix [[2,2,2],[4,5,6]])
      
    it "test matrix multplication" $ do
      mul (Matrix [[2,3,4],[4,5,6]]) (Matrix [[2,3],[4,5],[6,7]]) `shouldBe` (Matrix [[40,49],[64,79]])
      
    it "test power of matrix" $ do
      pow (Matrix [[2,3,4],[4,5,6]]) 3 `shouldBe` (Matrix [[8,27,64],[64,125,216]])
      
    it "test matrix exponential" $ do
      exponential (Matrix [[2,3,4],[4,5,6]]) `shouldBe` [[7.389056,20.085537,54.59815],[54.59815,148.41316,403.4288]]
      
    it "test matrix log" $ do
      matLog (Matrix [[2,3,4],[4,5,6]]) `shouldBe` [[0.6931472,1.0986123,1.3862944],[1.3862944,1.609438,1.7917595]]

    it "test matrix element wise absolute value" $ do
      matAbs (Matrix [[(-2),3, (-4)], [5, (-6), (-7)]]) `shouldBe` (Matrix [[2,3,4],[5,6,7]])

    it "test matrix maximum" $ do
       maximumm (Matrix [[1,1,1,1], [2,2,2,2]]) (Matrix [[2,2,2,2], [3,3,3,3]]) `shouldBe` (Matrix [[2,2,2,2],[3,3,3,3]])

    it "test matrix minimum" $ do
      minimumm (Matrix [[3,3,3,3],[4,5,6,7]]) (Matrix [[2,2,2,2],[2,2,2,2]]) `shouldBe` (Matrix [[2,2,2,2],[2,2,2,2]])

  describe "Vector addition" $ do
    it "test vector addition" $ do
      add (Vector [1,2,3]) (Vector [4,5,6]) `shouldBe` (Vector [5,7,9])

    it "test vector subtraction" $ do
      sub (Vector [4,5,6]) (Vector [2,3,4]) `shouldBe` (Vector [2,2,2])

    it "test vector element wise multiplication" $ do
      mul (Vector [2,3,4]) (Vector [2,3,4]) `shouldBe` (Vector [4, 9, 16])

    it "test vector power" $ do
      powv (Vector [2,3,4,5]) 2 `shouldBe` (Vector [4,9,16,25])

    it "test vector exponential" $ do
      exponentialv (Vector [2,3,4,5]) `shouldBe` [7.389056,20.085537,54.59815,148.41316]

    it "test vector log" $ do
      vecLog (Vector [2,3,4,5]) `shouldBe` [0.6931472,1.0986123,1.3862944,1.609438]

    it "test dot product" $ do
      dotP (Vector [3,4,5,6]) (Vector [5,6,7,8]) `shouldBe` 122

    it "test element wise absolute value" $ do
      vecAbs (Vector [(-3), 4, (-4)]) `shouldBe` (Vector [3,4,4])

    it  "test vector maximum" $ do
      maximumv (Vector [2,3,4,5]) (Vector [1,2,5,6]) `shouldBe` (Vector [2,3,5,6])

    it "test vector minimum" $ do
      minimumv (Vector [3,4,5]) (Vector [2,1,2]) `shouldBe` (Vector [2,1,2])

  describe "Vector-Matrix arithmetic" $ do
    it "test vector-matrix addition" $ do
      addMatrixVec [[2,3,4,5],[5,6,7,8]] [3,4,5,6] `shouldBe` [[5,7,9,11],[8,10,12,14]]

    it "test matrix-vector subtraction" $ do
      subMatrixVec [[2,3,4,5],[5,6,7,8]] [3,4,5,6] `shouldBe` [[-1,-1,-1,-1],[2,2,2,2]]

    it "test matrix-vector multiplication" $ do
      mulMatrixVec [[2,3,4,5],[5,6,7,8]] [3,4,5,6] `shouldBe` [68,122]
      
  describe "Polynomial arithmetic" $ do
    it "Polynomial addition" $ do
      add (Polynomial [3,4,5]) (Polynomial [2,3,2]) `shouldBe` (Polynomial [5,7,7])

      sub (Polynomial [5,4,3,2]) (Polynomial [2,2,2,2]) `shouldBe` (Polynomial [3,2,1,0])

      mul (Polynomial [3,4,5]) (Polynomial [6,8,10]) `shouldBe` (Polynomial [18,24,30,24,32,40,30,40,50])

  describe "Fraction arithmetic" $ do
    it "fraction addition" $ do
      add (Fraction 2 3) (Fraction 2 3) `shouldBe` (Fraction 12 9)

    it "fraction subtraction" $ do
     sub (Fraction 2 3) (Fraction 2 3) `shouldBe` (Fraction 0 9)

    it "fraction addition" $ do
     mul (Fraction 2 3) (Fraction 2 3) `shouldBe` (Fraction 4 9)
     
  describe "Neural networks" $ do
    it "softmax vector int test" $ do
      softmax (Vector [(-1), 0, 3, 5]) `shouldBe` (Vectorf [2.1656966e-3,5.8869733e-3,0.11824302,0.8737043])

    it "softmax vector float test" $ do
      softmax (Vectorf [3.2,4.5,2.1]) `shouldBe` (Vectorf [0.19991335,0.7335413,6.654536e-2])

    it "softmax matrix float test" $ do
      softmax2d (Matrixf [[2.3,-3.2,4.0],[2.3,-2.2,3.3]]) `shouldBe` (Matrixf [[0.15436782,6.30866e-4,0.84500134],[0.2681403,2.9787696e-3,0.72888094]])

    it "softmax matrix int test" $ do
      softmax2d (Matrix [[3,4,5,-3,-2],[3,4,5,-3,-2]]) `shouldBe` (Matrixf [[8.995593e-2,0.24452557,0.6646894,2.2297844e-4,6.061183e-4],[8.995593e-2,0.24452557,0.6646894,2.2297844e-4,6.061183e-4]])
      
    it "logsoftmax vector test" $ do
      logsoftmax (Vector [2,3,4,5,6]) `shouldBe` (Vectorf [-4.4519143,-3.4519143,-2.4519143,-1.4519143,-0.45191434])

    it "logsoftmax matrix float test" $ do
      logsoftmax2d (Matrixf [[2.3,-3.2,4.0],[2.3,-2.2,3.3]]) `shouldBe` (Matrixf [[-1.8684171,-7.3684173,-0.16841707],[-1.316245,-5.816245,-0.31624487]])

    it "logsoftmax matrix int test" $ do
      logsoftmax2d (Matrix [[3,4,5,-3,-2],[3,4,5,-3,-2]]) `shouldBe` (Matrixf [[-2.4084353,-1.4084355,-0.40843537,-8.408436,-7.4084353],[-2.4084353,-1.4084355,-0.40843537,-8.408436,-7.4084353]])


    it "logsoftmax float test" $ do
      logsoftmax (Vectorf [3.2,4.5,2.1])`shouldBe` (Vectorf [-1.6098713,-0.30987138,-2.7098715])

    it "relu float vec test" $ do
      relu (Vectorf [3.2,4.5,(-2.3)]) `shouldBe` (Vecf [3.2,4.5,0])

    it "relu Int vec test" $ do
      relu (Vector [2,3,(-3), (-4)]) `shouldBe` (Vec [2,3,0,0])

    it "relu Float matrix test" $ do
      relu2d (Matrixf [[2.3, (-3.2), 4], [2.3, (-2.2), 3.3]]) `shouldBe` (Matf [[2.3,0.0,4.0],[2.3,0.0,3.3]])

    it "relu Int matrix test" $ do
      relu2d (Matrix [[3,4,5,(-3), (-2)], [3,4,5,(-3), (-2)]]) `shouldBe` (Mat [[3,4,5,0,0],[3,4,5,0,0]])

    it "sigmoid element wise matrix Float test" $ do
      sigmoid2d (Matrixf [[3.2,3.2,2.1], [3.2,3.2,2.1]]) `shouldBe` (Matrixf [[0.96083426,0.96083426,0.8909032],[0.96083426,0.96083426,0.8909032]])

    it "sigmoid element wise matrix Int test" $ do
      sigmoid2d (Matrix [[2,1,2,3], [3,4,5,6]]) `shouldBe` (Matrixf [[0.880797,0.7310586,0.880797,0.95257413],[0.95257413,0.98201376,0.9933072,0.9975274]])

    it "sigmoid element wise vector Int test" $ do
      sigmoid (Vector [2,3,4]) `shouldBe` (Vectorf [0.880797,0.95257413,0.98201376])

    it "sigmoid element wise vector float test" $ do
      sigmoid (Vectorf [3.2,2.1,3.4,5.6]) `shouldBe` (Vectorf [0.96083426,0.8909032,0.9677046,0.9963158])

    it "tanh element wise vector Float test" $ do
      NeuralNetworks.tanh (Vectorf [2.0, (- 0.4), 1.1, (- 2.0), (- 5.4)]) `shouldBe` (Vectorf [0.9640275,-0.379949,0.8004991,-0.9640275,-0.99995923])

    it "tanh element wise vector Int test" $ do
      NeuralNetworks.tanh (Vector [2,4,5,6]) `shouldBe` (Vectorf [0.9640275,0.9993293,0.9999091,0.9999877])

    it "tanh element wise matrix Float test" $ do
      NeuralNetworks.tanh2d (Matrixf [[1.2,3.5,3.2],[3.2,3.4,2.1]]) `shouldBe` (Matrixf [[0.83365464,0.9981779,0.9966824],[0.9966824,0.99777496,0.9704519]])

    it "tanh element wise matrix Int test" $ do
      NeuralNetworks.tanh2d (Matrix [[1,2,3],[4,5,6]]) `shouldBe` (Matrixf [[0.7615942,0.9640275,0.9950547],[0.9993293,0.9999091,0.9999877]])

    it "1d convolution for Ints" $ do
      NeuralNetworks.convolution (Vector [1,2,3]) (Vector [4,5,6]) `shouldBe` (Vector [4,13,28,27,18])

    it "id Float convolution" $ do
      NeuralNetworks.convolution (Vectorf [1.0, 2.0, 3.0]) (Vectorf [4.0, 5.0, 6.0]) `shouldBe` (Vectorf [4.0,13.0,28.0,27.0,18.0])
