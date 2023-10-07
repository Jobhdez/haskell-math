module NeuralNetworks where
{-
class NN a where
  softmax :: a -> [Float]
  logsoftmax :: a -> [Float]

class NNf a where
  softmaxf :: a -> a
  logsoftmaxf :: a -> a

instance NN [Int] where
  softmax = softmax'
  logsoftmax = logsoftmax'

instance NN [[Int]] where
  softmax = softmax2d
  logsoftmax = logsoftmax2d

instance NNf [Float] where
  softmaxf = softmaxf'
  logsoftmaxf = logsoftmaxf'

instance NNf [[Float]] where
  softmaxf = softmaxf2d
  logsoftmaxf = logsoftmaxf2d
-}

softmax' :: [Int] -> [Float]
softmax' vec =
  let expVec = map (\x -> exp (fromIntegral x)) vec
      sumExps = sum expVec
  in
    map (\x -> x / sumExps) expVec
  

logsoftmax' :: [Int] -> [Float]
logsoftmax' vec =
  let smax = softmax' vec
  in
    map (\x -> log x) smax
    
    
softmax2d :: [[Int]] -> [[Float]]
softmax2d matrix =
  map (\x -> softmax' x) matrix

logsoftmax2d :: [[Int]] -> [[Float]]
logsoftmax2d matrix =
  map (\x -> logsoftmax' x) matrix

softmaxf' :: [Float] -> [Float]
softmaxf' vec =
  let expVec = map (\x -> exp x) vec
      sumExps = sum expVec
  in
    map (\x -> x / sumExps) expVec

logsoftmaxf' :: [Float] -> [Float]
logsoftmaxf' vec =
  let smax = softmaxf' vec
  in
    map (\x -> log x) smax


softmaxf2d :: [[Float]] -> [[Float]]
softmaxf2d matrix =
  map (\x -> softmaxf' x) matrix

logsoftmaxf2d :: [[Float]] -> [[Float]]
logsoftmaxf2d matrix =
  map (\x -> logsoftmaxf' x) matrix
