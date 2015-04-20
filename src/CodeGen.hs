module CodeGen where

import qualified Data.Map as Map
import MTypes

-----------------------------------------------------
-- Code generation for numpy arrays

generateNumpy :: Expr -> String
generateNumpy (Leaf a) = a
generateNumpy (IdentityLeaf n) = "np.eye(" ++ (show n) ++ ")"
generateNumpy (LiteralScalar x) = let ix = round x
                                      isInt = x == (fromIntegral ix) in
                                      if isInt then show ix else show x
generateNumpy (Branch3 MTernaryProduct t1 t2 t3) = "np.dot(np.dot(" ++ (generateNumpy t1) ++ 
                                                   ", " ++ (generateNumpy t2)  ++ "), " ++ 
                                                   (generateNumpy t3) ++ ")"
generateNumpy (Branch2 MLinSolve t1 t2) = "np.linalg.solve(" ++ (generateNumpy t1) ++ 
                                          ", " ++ (generateNumpy t2)  ++ ")" 
generateNumpy (Branch2 MCholSolve t1 t2) = "scipy.linalg.cho_solve((" ++ (generateNumpy t1) ++ 
                                          ", True), " ++ (generateNumpy t2)  ++ ")" 
generateNumpy (Branch2 MProduct t1 t2) = "np.dot(" ++ (generateNumpy t1) ++ 
                                          ", " ++ (generateNumpy t2)  ++ ")" 
generateNumpy (Branch2 MScalarProduct t1 t2) = "( " ++ (generateNumpy t1) ++ " * "
                                                  ++ (generateNumpy t2)  ++ ")" 
generateNumpy (Branch2 MSum t1 (Branch1 MNegate t2)) = (generateNumpy t1) ++ " - " ++ (generateNumpy t2)
generateNumpy (Branch2 MSum t1 t2) = (generateNumpy t1) ++ " + " ++ (generateNumpy t2)
generateNumpy (Branch1 MInverse t) = "np.linalg.inv(" ++ (generateNumpy t) ++ ")"
generateNumpy (Branch1 MTranspose t) = (generateNumpy t) ++ ".T" -- caution: might we need parentheses here?
generateNumpy (Branch1 MNegate t) = "-" ++ (generateNumpy t)
generateNumpy (Branch1 MChol t) = "np.linalg.cholesky(" ++ (generateNumpy t) ++ ")"
generateNumpy (Branch1 MTrace t) = "np.trace(" ++ (generateNumpy t) ++ ")"
generateNumpy (Branch1 MDet t) = "np.linalg.det(" ++ (generateNumpy t) ++ ")"
generateNumpy (Branch1 MDiagMV t) = "np.diag(" ++ (generateNumpy t) ++ ")"
generateNumpy (Branch1 MDiagVM t) = "np.diag(" ++ (generateNumpy t) ++ ")"

-- we use "empty" let expressions do denote the final quantitity to be computed, but we don't actually
-- need to generate a body for such expressions
generateNumpy (Let lhs rhs tmp (Leaf _)) = lhs ++ " = " ++ (generateNumpy rhs) 
generateNumpy (Let lhs rhs tmp body) = lhs ++ " = " ++ (generateNumpy rhs) ++ "\n" ++ (generateNumpy body)

---------------------------------------------------------------------------------------------------
-- Code generation for MATLAB

generateMatlab :: Expr -> String
generateMatlab (Leaf a) = a
generateMatlab (IdentityLeaf n) = "eye(" ++ (show n) ++ ")"
generateMatlab (LiteralScalar x) = let ix = round x
                                       isInt = x == (fromIntegral ix) in
                                       if isInt then show ix else show x
generateMatlab (Branch3 MTernaryProduct t1 t2 t3) = "(" ++ (generateMatlab t1) ++ " * " ++
                                                        (generateMatlab t2)  ++ " * " ++
                                                        (generateMatlab t3) ++ ")"
generateMatlab (Branch2 MLinSolve t1 t2) = "(" ++ (generateMatlab t1) ++
                                          "\\" ++ (generateMatlab t2)  ++ ")"
generateMatlab (Branch2 MCholSolve t1 t2) = "(" ++ (generateMatlab t1) ++
                                            "\\((" ++ (generateMatlab t1) ++
                                            ")'\\" ++ (generateMatlab t2) ++ "))"
generateMatlab (Branch2 MProduct t1 t2) = "(" ++ (generateMatlab t1) ++
                                          " * " ++ (generateMatlab t2)  ++ ")"
generateMatlab (Branch2 MScalarProduct t1 t2) = "(" ++ (generateMatlab t1) ++
                                                " * " ++ (generateMatlab t2)  ++ ")"
generateMatlab (Branch2 MSum t1 (Branch1 MNegate t2)) = (generateMatlab t1) ++ " - " ++ (generateMatlab t2)
generateMatlab (Branch2 MSum t1 t2) = "(" ++ (generateMatlab t1) ++ " + " ++ (generateMatlab t2) ++ ")"
generateMatlab (Branch1 MInverse t) = "inv(" ++ (generateMatlab t) ++ ")"
generateMatlab (Branch1 MTranspose t) = (generateMatlab t) ++ "'" -- caution: might we need parentheses here?
generateMatlab (Branch1 MNegate t) = "-" ++ (generateMatlab t)
generateMatlab (Branch1 MChol t) = "chol(" ++ (generateMatlab t) ++ ")"
generateMatlab (Branch1 MTrace t) = "trace(" ++ (generateMatlab t) ++ ")"
generateMatlab (Branch1 MDet t) = "det(" ++ (generateMatlab t) ++ ")"
generateMatlab (Branch1 MTrace t) = "diag(" ++ (generateMatlab t) ++ ")"
generateMatlab (Branch1 MDet t) = "diag(" ++ (generateMatlab t) ++ ")"

generateMatlab (Let lhs rhs tmp (Leaf _)) = lhs ++ " = " ++ (generateMatlab rhs)  ++ "\n"
generateMatlab (Let lhs rhs tmp body) = lhs ++ " = " ++ (generateMatlab rhs) ++ "\n" ++ (generateMatlab body) ++ ";"

symTableMatlab :: SymbolTable -> String
symTableMatlab tbl = foldl (++)  "" [tblEntry k m | (k,m) <- Map.toList tbl]  where
  tblEntry k (Matrix n m _) = k ++ " = randn(" ++ (show n) ++ ", " ++ (show m) ++ ");\n"