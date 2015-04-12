module CodeGen where


import MTypes

-----------------------------------------------------
-- Code generation for numpy arrays

generateNumpy :: Expr -> String
generateNumpy (Leaf a) = a
generateNumpy (IdentityLeaf n) = "np.eye(" ++ (show n) ++ ")"
generateNumpy (Branch3 MTernaryProduct t1 t2 t3) = "np.dot(np.dot(" ++ (generateNumpy t1) ++ 
                                                   ", " ++ (generateNumpy t2)  ++ "), " ++ 
                                                   (generateNumpy t3) ++ ")"
generateNumpy (Branch2 MLinSolve t1 t2) = "np.linalg.solve(" ++ (generateNumpy t1) ++ 
                                          ", " ++ (generateNumpy t2)  ++ ")" 
generateNumpy (Branch2 MCholSolve t1 t2) = "scipy.linalg.cho_solve(scipy.linalg.cho_factor(" ++ (generateNumpy t1) ++ 
                                          "), " ++ (generateNumpy t2)  ++ ")" 
generateNumpy (Branch2 MProduct t1 t2) = "np.dot(" ++ (generateNumpy t1) ++ 
                                          ", " ++ (generateNumpy t2)  ++ ")" 
generateNumpy (Branch2 MSum t1 t2) = (generateNumpy t1) ++ " + " ++ (generateNumpy t2)
generateNumpy (Branch1 MInverse t) = "np.linalg.inv(" ++ (generateNumpy t) ++ ")"
generateNumpy (Branch1 MTranspose t) = (generateNumpy t) ++ ".T" -- caution: might we need parentheses here?
generateNumpy (Branch1 MNegate t) = "-" ++ (generateNumpy t)

-- we use "empty" let expressions do denote the final quantitity to be computed, but we don't actually
-- need to generate a body for such expressions
generateNumpy (Let lhs rhs tmp (Leaf _)) = lhs ++ " = " ++ (generateNumpy rhs) 
generateNumpy (Let lhs rhs tmp body) = lhs ++ " = " ++ (generateNumpy rhs) ++ "\n" ++ (generateNumpy body)