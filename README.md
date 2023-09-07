# haskell-Arith
just a simple exercise to learn more about haskell.

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
- dot product

## Linear Algebra
- determinant of 3x3 matrices
- trace 
- upper triangular
- lower triangular matrices

## Polynomial
- addition
- subtraction
- multiplication

# Tests
```
* cabal test
```

# Reflection
This system is inspired by the symbolic algebra system in  SICP -- so type classes offer the benefits of data directed programming. In data directed programming one implements packages for the different type of math objects so each package can be implemented independently by multiple programmers and without any worries about name conflicts. This leads to a modular system and leads to better maintenance. Type classes offer something similar, I think, because by implementing a typle class, multiple programmers can work on a seperate instance of the type class independently. And by using modules in Haskell you also do not have to worry about having name conflicts, I think. It may be because I am fairly new to Haskell but although type classes allow you to organize your system in a modular way it is kind of an inconvenience - suppose your type class consists of 100 functions that each instance of the type class has to implement. If one needs to implement the whole 100 functions for each instance then this will lead to poor interative development. The time it takes to debug each instance will grow linearly with the number of functions of the given type class. In common lisp I am use to writing one function and testing it right away in the repl. Speaking of repls: The haskell repl is not conducive to good interactive development; for example, everytime I reload a file I need to start a new repl session, I need to redefine all my work.
