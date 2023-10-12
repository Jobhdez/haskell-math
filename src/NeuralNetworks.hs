module NeuralNetworks where

import Matrix 
import Vector 

data Vectorf = Vectorf [Float] deriving (Show, Eq)
data Matrixf = Matrixf [[Float]] deriving (Show, Eq)
data VectorExp = Vec [Int] | Vecf [Float] deriving (Show, Eq)
data MatrixExp = Mat [[Int]] | Matf [[Float]] deriving (Show, Eq)

getVector :: Vector -> [Int]
getVector (Vector vec) = vec

getVectorf :: Vectorf -> [Float]
getVectorf (Vectorf vf) = vf

class NN a where
  softmax :: a -> Vectorf
  logsoftmax :: a -> Vectorf
  relu :: a -> VectorExp
  sigmoid :: a -> Vectorf

instance NN Vector where
  softmax = softmax'
  logsoftmax = logsoftmax'
  relu = reluV
  sigmoid = sigmoid'

instance NN Vectorf where
  softmax = softmaxf'
  logsoftmax = logsoftmaxf'
  relu = reluVf
  sigmoid = sigmoidf'

class NN2D a where
  softmax2d :: a -> Matrixf
  logsoftmax2d :: a -> Matrixf
  relu2d :: a -> MatrixExp
  sigmoid2d :: a -> Matrixf

instance NN2D Matrix where
  softmax2d = softmax2d'
  logsoftmax2d = logsoftmax2d'
  relu2d = reluM
  sigmoid2d = sigmoid2d'

instance NN2D Matrixf where
  softmax2d = softmaxf2d
  logsoftmax2d = logsoftmaxf2d
  relu2d = reluMf
  sigmoid2d = sigmoid2df'

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


reluM :: Matrix -> MatrixExp
reluM (Matrix mat) =
  (Mat m)
  where
    m = map(\x -> map(\x ->  relu' x) x) mat

reluMf :: Matrixf -> MatrixExp
reluMf (Matrixf mat) =
  (Matf m)
  where
    m = map(\x -> map(\x -> reluf' x) x) mat

reluV :: Vector -> VectorExp
reluV (Vector vec) =
  (Vec v)
  where
    v = map(\x ->  relu' x) vec

reluVf :: Vectorf -> VectorExp
reluVf (Vectorf vec) =
  (Vecf v)
  where
    v = map(\x ->  reluf' x) vec

relu' :: Int -> Int
relu' n = max 0 n

reluf' :: Float -> Float
reluf' n = max 0 n

sigmoid' :: Vector -> Vectorf
sigmoid' (Vector vec) =
 (Vectorf v2)
 where
   v2 = map (\x -> sigmoidfn x) vec

sigmoidf' :: Vectorf -> Vectorf
sigmoidf' (Vectorf v) =
  (Vectorf v2) 
  where
    v2 = map (\x -> sigmoidfnf x) v

sigmoid2d' :: Matrix -> Matrixf
sigmoid2d' (Matrix mat) =
  (Matrixf mat2)
  where
    mat2 = map (\x -> map (\x -> sigmoidfn x) x) mat

sigmoid2df' :: Matrixf -> Matrixf
sigmoid2df' (Matrixf mat) =
  (Matrixf mat2)
  where
    mat2 = map (\x -> map (\x -> sigmoidfnf x) x) mat

sigmoidfn :: Int -> Float
sigmoidfn n =
  (fromIntegral 1) / ((fromIntegral 1) + exp (- (fromIntegral n)))

sigmoidfnf :: Float -> Float
sigmoidfnf n =
  (fromIntegral 1) /  ((fromIntegral 1) + exp (- n))

tanh' :: Vector -> Vectorf
tanh' (Vector vec) =
  (Vectorf v)
  where
    v = map (\x -> tanhfn x) vec

tanhf' :: Vectorf -> Vectorf
tanhf' (Vectorf vec) =
  (Vectorf v)
  where
    v = map (\x -> tanhfnf x) vec
  
tanhfn :: Int -> Float
tanhfn n =
  ((exp (fromIntegral n)) - (exp (fromIntegral (- n)))) / ((exp (fromIntegral n)) + (exp (fromIntegral (- n))))

tanhfnf :: Float -> Float
tanhfnf n =
  ((exp n) - (exp (-n))) / ((exp n) + (exp (- n)))
