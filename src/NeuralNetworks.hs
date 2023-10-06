module NeuralNetworks where

softmax :: [Int] -> [Float]
softmax vec =
  let expVec = map (\x -> exp (fromIntegral x)) vec
      sumExps = sum expVec
  in
    map (\x -> x / sumExps) expVec
  

logsoftmax :: [Int] -> [Float]
logsoftmax vec =
  let smax = softmax vec
  in
    map (\x -> log x) smax
    
    
