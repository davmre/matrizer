matrizer
========

matrizer is an optimizing compiler for matrix expressions. You give it an expression (currently in awkward custom MATLAB-like syntax), along with approximate sizes of the matrices involved, and it applies algebraic identities along with other optimizations to search for a version of the expression requiring fewer FLOPs to compute. The result is output as code generated for your favorite numeric computing environment (currently only Python/numpy is implemented). For example, the normal equations expression 
 
    (X'X)^-1 X'y 

is transformed into Python code 

    scipy.linalg.cho_solve(scipy.linalg.cho_factor(np.dot(X.T, X)), np.dot(X.T, y)).

Setup
-----

To build:

    runhaskell Setup.hs configure
    runhaskell Setup.hs build

If cabal complains about being unable to find a dependency that you
know is installed, it might be in your user package db. Try running

    runhaskell Setup.hs --user configure

instead. For other common issues with cabal, see
http://www.haskell.org/cabal/FAQ.html.

Try running an example:

    dist/build/matrizer/matrizer examples/normaleqns.mtr

This should output:

    Preamble symbol table: fromList [("X",100x5 []),("y",100x1 [])]
    Code parsed as:
    w := (* (* (inv (* (transpose X) X)) (transpose X)) y)
    Inferred symbol table: fromList [("X",100x5 []),("w",5x1 []),("y",100x1 [])]
    Naive FLOPs required: 10563
    Naive code generated:
    w = np.dot(np.dot(np.linalg.inv(np.dot(X.T, X)), X.T), y)

    Optimized flops required: 6061
    Optimized program:
    w := (cholSolve (* (transpose X) X) (* (transpose X) y))
    Optimized code generated:
    w = scipy.linalg.cho_solve(scipy.linalg.cho_factor(np.dot(X.T, X)), np.dot(X.T, y))

There is currently no documentation, but the examples provide a
reasonable guide to what's possible.  Currently we support basic
matrix operations (addition, multiplication, inverse, transpose,
negation, solving linear systems using LU or Cholesky decompositions),
matrix properties (diagonal, symmetric, positive definite), and many
of the obvious algebraic rewrite rules.

See the file TODO for features we hope to implement.