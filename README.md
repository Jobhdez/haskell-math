# Mathy
A math web service written in Haskell

# Installation
```
* git clone git@github.com:Jobhdez/mathy-webservice.git
```

# Running the api
```
* cabal build
* cabal run haskell-Arith
```

## Examples

```
$ curl -X POST -d '{"expr": [[2,3,4],[4,6,7],[5,6,7]]}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/api/matrix/det
{"exp":-3}

$ curl -X POST -d '{"expr": [[2,3,4],[4,6,7],[5,6,7]]}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/api/matrix/matTrace
{"traceExp":15}

$ curl -X POST -d '{"expr": [[2,3,4],[4,3,7],[5,4,7]]}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/api/matrix/matUpTriangular
{
    "expr": [
        [2, 3, 4],
        [0, 3, 7],
        [0, 0, 7]
    ]
}

$ curl -X POST -d '{"expr": [[2,3,4],[4,3,7],[5,4,7]]}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/api/matrix/matLowTriangular
{
    "expr": [
        [2, 0, 0],
        [4, 3, 0],
        [5, 4, 7]
    ]
}

$ curl -X POST -d '{"mexp": [[2,3,4],[4,6,7],[5,6,7]], "mexp2": [[1,1,1],[1,1,1],[1,1,1]]}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/api/matrix/arithmetic/addition

$ curl -X POST -d '{"mexp": [[2,3,4],[4,6,7],[5,6,7]], "mexp2": [[1,1,1],[1,1,1],[1,1,1]]}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/api/matrix/arithmetic/subtraction

$ curl -X POST -d '{"mexp": [[2,3,4],[4,6,7]], "mexp2": [[1,1],[1,1],[1,1]]}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/api/matrix/arithmetic/multiplication

$ curl -X GET -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/api/matrix/arithmetic/exps

```

# Running the tests

```
$ cabal test
```

# Math supported

## Matrix
- addition
- subtraction
- multiplication
- element wise power
- element wise exponential
- element wise log
- element wise absolute value
- maximum
- minimum

## Vector
- addition
- subtraction
- element wise multiplication
- element wise power
- element wise exponential
- element wise log
- element wise absolute value
- maximum
- minimum

## Linear Algebra
- determinant of 3x3 matrices
- trace 
- upper triangular
- lower triangular

## Neural Networks
*Data Types supported*: `[Int]`, `[Float]`, `[[Int]]`, `[[Float]]`
- softmax
- logsoftmax
- softmax2d
- logsoftmax2d
- relu
- relu2d
- sigmoid
- sigmoid2d
- tanh
- tanh2d
- convolution

## Matrix-Vec
- matrix-vec addition
- matrix-vec subtraction
- matrix-vec multiplication

## Polynomial
- addition
- subtraction
- multiplication
  
## Fraction
- addition
- subtraction
- multiplication

# Troubleshoot
```
$ lsof -i:8081
$ kill $(lsof -t -i:8081)
```

