# haskell-Arith
just a simple toy exercise to learn more about haskell.

# Functionality

## Matrix
- addition
- subtraction
- multiplication
- power
- exponential
- log

## Vector
- addition
- subtraction
- element wise multiplication
- power
- exponential
- log

## Linear Algebra
- determinant of 3x3 matrices
- trace 
- upper triangular
- lower triangular matrices

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
