# haskell-Arith
just a simple toy exercise to learn more about haskell.

# Functionality

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
- element wise absolute value'
- maximum
- minimum

## Linear Algebra
- determinant of 3x3 matrices
- trace 
- upper triangular
- lower triangular

## Neural Networks
- 1D softmax
- 1D logsoftmax

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

# API
```
$ cabal run haskell-Arith

$ curl -X POST -d '{"expr": [[2,3,4],[4,6,7],[5,6,7]]}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/det
{"exp":-3}

$ curl -X POST -d '{"expr": [[2,3,4],[4,6,7],[5,6,7]]}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/matTrace
{"traceExp":15}

$ curl -X POST -d '{"expr": [[2,3,4],[4,3,7],[5,4,7]]}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/matUpTriangular
{
    "expr": [
        [2, 3, 4],
        [0, 3, 7],
        [0, 0, 7]
    ]
}

$ curl -X POST -d '{"expr": [[2,3,4],[4,3,7],[5,4,7]]}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/matLowTriangular
{
    "expr": [
        [2, 0, 0],
        [4, 3, 0],
        [5, 4, 7]
    ]
}
```
## Troubleshoot
```
$ lsof -i:8081
$ kill $(lsof -t -i:8081)
```
# Tests
```
$ cabal test
```

thanks
