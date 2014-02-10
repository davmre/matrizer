matrizer
========

To build, run:

ghc --make Matrizer

Try an example: ./Matrizer examples/normaleqns.mtr

This should output:

     Symbol table: fromList [('X',100x5 []),('y',100x1 [])]
     Parsed as: (* (* (inv (* (transpose X) X)) (transpose X)) y)
     Resulting matrix: 5x1 []
     Naive FLOPs required: 10563
     Naive code generated: np.dot(np.dot(np.linalg.inv(np.dot(X.T, X)), X.T), y)

     Optimized flops required: 6061
     Optimized tree: (cholSolve (* (transpose X) X) (* (transpose X) y))
     Optimized code: scipy.linalg.cho_solve(scipy.linalg.cho_factor(np.dot(X.T, X)), np.dot(X.T, y))

There is currently no documentation, but the examples provide a
reasonable guide to what's possible.  Currently we support basic
matrix operations (addition, multiplication, inverse, transpose,
negation, solving linear systems using LU or Cholesky decompositions),
matrix properties (diagonal, symmetric, positive definite), and many
of the obvious algebraic rewrite rules.

See the file TODO for features we hope to implement.