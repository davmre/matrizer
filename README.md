matrizer
========

matrizer is an optimizing compiler for matrix expressions. You give it an expression (currently in awkward custom MATLAB-like syntax), along with approximate sizes of the matrices involved, and it applies algebraic identities along with other optimizations to search for a version of the expression requiring fewer FLOPs to compute. The result is output as code generated for your favorite numeric computing environment (currently Python/numpy and MATLAB are implemented). For example, the normal equations expression 
 
    (X'X)^-1 X'y 

is transformed into Python code 

    scipy.linalg.cho_solve(scipy.linalg.cho_factor(np.dot(X.T, X)), np.dot(X.T, y)).

*Beware:* this is still highly unpolished alpha-level software. Many
features are missing, and there is no guarantee that the expressions
output are optimal or even correct. It's best to think of the results
as a source of inspiration rather than something to be blindly
trusted.

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

    dist/build/matrizer/matrizer tests/normaleqns.mtr

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

There is currently no documentation, but the test cases provide a
reasonable guide to what's possible.  Currently we support basic
matrix operations (addition, multiplication, inverse, transpose,
negation, solving linear systems using LU or Cholesky decompositions),
matrix properties (diagonal, symmetric, positive definite), and many
of the obvious algebraic rewrite rules.

See the file TODO for features we hope to implement.

Running tests
-----

Test cases are stored in the tests/ folder, with the convention that
each file containing a test case is accompanied by another file with
the "_soln" suffix containing a 'gold-standard' optimized version. 

The command

    dist/build/matrizer/matrizer test

runs the optimizer on all test cases, comparing FLOP counts against 
the provided solutions. 

To test for semantic correctness and/or real-world speed, the commands

    dist/build/matrizer/matrizer genpython
    python tests/test_generated_python.py

will generate Python source code for the naive and optimized versions
of each test case, using random Gaussian matrices, and compare the
results. Note that numeric instability may sometimes cause two
semantically-equivalent programs to yield different results on real
matrices. Often the optimized program is more stable than the original,
since fewer operations means fewer chances for numerical blowup, but 
there are no guarantees. 



