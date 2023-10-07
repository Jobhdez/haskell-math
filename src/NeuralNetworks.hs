module NeuralNetworks where

import Matrix 
import Vector 

data Vectorf = Vectorf [Float] deriving (Show, Eq)
data Matrixf = Matrixf [[Float]] deriving (Show, Eq)

getVector :: Vector -> [Int]
getVector (Vector vec) = vec

getVectorf :: Vectorf -> [Float]
getVectorf (Vectorf vf) = vf

class NN a where
  softmax :: a -> Vectorf
  logsoftmax :: a -> Vectorf

instance NN Vector where
  softmax = softmax'
  logsoftmax = logsoftmax'

instance NN Vectorf where
  softmax = softmaxf'
  logsoftmax = logsoftmaxf'

class NN2D a where
  softmax2d :: a -> Matrixf
  logsoftmax2d :: a -> Matrixf

instance NN2D Matrix where
  softmax2d = softmax2d'
  logsoftmax2d = logsoftmax2d'

instance NN2D Matrixf where
  softmax2d = softmaxf2d
  logsoftmax2d = logsoftmaxf2d

softmax' :: Vector -> Vectorf
softmax' (Vector vec) =
  let expVec = map (\x -> exp (fromIntegral x)) vec
      sumExps = sum expVec
      fvec = map (\x -> x / sumExps) expVec
  in
    (Vectorf fvec)
  

logsoftmax' :: Vector -> Vectorf
logsoftmax' (Vector vec) =
  let smax = softmax' (Vector vec)
      fvec = map (\x -> log x) (getVectorf smax)
  in
    (Vectorf fvec)
    
softmax2d' :: Matrix -> Matrixf
softmax2d' (Matrix matrix) =
  (Matrixf matf)
  where
    matf = map (\x -> (getVectorf (softmax' (Vector x)))) matrix

logsoftmax2d' :: Matrix -> Matrixf
logsoftmax2d' (Matrix matrix) =
  (Matrixf matf)
  where
    matf = map (\x -> (getVectorf (logsoftmax' (Vector x)))) matrix

softmaxf' :: Vectorf -> Vectorf
softmaxf' (Vectorf vec) =
  let expVec = map (\x -> exp x) vec
      sumExps = sum expVec
      fvec = map (\x -> x / sumExps) expVec
  in
    (Vectorf fvec)
  
logsoftmaxf' :: Vectorf -> Vectorf
logsoftmaxf' (Vectorf vec) =
  let smax = softmaxf' (Vectorf vec)
      fvec = map (\x -> log x) (getVectorf smax)

  in
    (Vectorf fvec)
    

softmaxf2d :: Matrixf -> Matrixf
softmaxf2d (Matrixf matrix) =
  (Matrixf matf)
  where
    matf = map (\x -> (getVectorf (softmaxf' (Vectorf x)))) matrix

logsoftmaxf2d :: Matrixf -> Matrixf
logsoftmaxf2d (Matrixf matrix) =
  (Matrixf matf)
  where
    matf = map (\x -> (getVectorf (logsoftmaxf' (Vectorf x)))) matrix

