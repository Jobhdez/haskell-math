module Tensor where

data Tensor = Scalar Int | Tensor [Tensor] deriving (Show, Eq)


--- [[[3,4,6],[5,7,7]], [[4,7,8],[9,8,7]]]
