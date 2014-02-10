matrizer
========

To build, run:

ghc --make Matrizer

Try an example: ./Matrizer examples/normaleqns.mtr
Should output:

     Symbol table: fromList [('X',100x5 []),('y',100x1 [])]
     Parsed as: (* (* (inv (* (transpose X) X)) (transpose X)) y)
     Resulting matrix: 5x1 []
     Naive FLOPs required: 10563
     Naive code generated: np.dot(np.dot(np.linalg.inv(np.dot(X.T, X)), X.T), y)

     Optimized flops required: 6061
     Optimized tree: (cholSolve (* (transpose X) X) (* (transpose X) y))
     Optimized code: scipy.linalg.cho_solve(scipy.linalg.cho_factor(np.dot(X.T, X)), np.dot(X.T, y))

There is currently no documentation, but the examples should provide a reasonable guide to what's possible.



