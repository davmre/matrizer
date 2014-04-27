module CodeGen where


import MTypes

-----------------------------------------------------
-- Code generation for numpy arrays

generateNumpy :: Program -> String
generateNumpy (Seq (x:[])) = generateNumpyStmt x
generateNumpy (Seq (x:xs)) = generateNumpyStmt x ++ "\n" ++ generateNumpy (Seq $ xs)
generateNumpy (Seq []) = ""

generateNumpyStmt :: Stmt -> String
generateNumpyStmt (Assign v e) = [v] ++ " = " ++ generateNumpyExpr e

generateNumpyExpr :: Expr -> String
generateNumpyExpr (Leaf a) = [a]
generateNumpyExpr (Branch3 MTernaryProduct t1 t2 t3) = "np.dot(np.dot(" ++ (generateNumpyExpr t1) ++ 
                                                   ", " ++ (generateNumpyExpr t2)  ++ "), " ++ 
                                                   (generateNumpyExpr t3) ++ ")"
generateNumpyExpr (Branch2 MLinSolve t1 t2) = "np.linalg.solve(" ++ (generateNumpyExpr t1) ++ 
                                          ", " ++ (generateNumpyExpr t2)  ++ ")" 
generateNumpyExpr (Branch2 MCholSolve t1 t2) = "scipy.linalg.cho_solve(scipy.linalg.cho_factor(" ++ (generateNumpyExpr t1) ++ 
                                          "), " ++ (generateNumpyExpr t2)  ++ ")" 
generateNumpyExpr (Branch2 MProduct t1 t2) = "np.dot(" ++ (generateNumpyExpr t1) ++ 
                                          ", " ++ (generateNumpyExpr t2)  ++ ")" 
generateNumpyExpr (Branch2 MSum t1 t2) = (generateNumpyExpr t1) ++ " + " ++ (generateNumpyExpr t2)
generateNumpyExpr (Branch1 MInverse t) = "np.linalg.inv(" ++ (generateNumpyExpr t) ++ ")"
generateNumpyExpr (Branch1 MTranspose t) = (generateNumpyExpr t) ++ ".T" -- caution: might we need parentheses here?
generateNumpyExpr (Branch1 MNegate t) = "-" ++ (generateNumpyExpr t)
