module CodeGen where

import qualified Data.Map as Map
import MTypes

-----------------------------------------------------
-- Code generation for numpy arrays

generateNumpy :: Program -> String
generateNumpy (Seq (x:[])) = generateNumpyStmt x
generateNumpy (Seq (x:xs)) = generateNumpyStmt x ++ "\n" ++ generateNumpy (Seq $ xs)
generateNumpy (Seq []) = ""

generateNumpyStmt :: Stmt -> String
generateNumpyStmt (Assign v e _) = v ++ " = " ++ generateNumpyExpr e

generateNumpyExpr :: Expr -> String
generateNumpyExpr (Leaf a) = a
generateNumpyExpr (IdentityLeaf n) = "np.eye(" ++ (show n) ++ ")"
generateNumpyExpr (Branch3 MTernaryProduct t1 t2 t3) = "np.dot(np.dot(" ++ (generateNumpyExpr t1) ++
                                                   ", " ++ (generateNumpyExpr t2)  ++ "), " ++
                                                   (generateNumpyExpr t3) ++ ")"
generateNumpyExpr (Branch2 MLinSolve t1 t2) = "np.linalg.solve(" ++ (generateNumpyExpr t1) ++
                                          ", " ++ (generateNumpyExpr t2)  ++ ")"
generateNumpyExpr (Branch2 MCholSolve t1 t2) = "scipy.linalg.cho_solve(" ++ (generateNumpyExpr t1) ++
                                          ", " ++ (generateNumpyExpr t2)  ++ ")"
generateNumpyExpr (Branch2 MProduct t1 t2) = "np.dot(" ++ (generateNumpyExpr t1) ++
                                          ", " ++ (generateNumpyExpr t2)  ++ ")"
generateNumpyExpr (Branch2 MSum t1 t2) = (generateNumpyExpr t1) ++ " + " ++ (generateNumpyExpr t2)
generateNumpyExpr (Branch1 MInverse t) = "np.linalg.inv(" ++ (generateNumpyExpr t) ++ ")"
generateNumpyExpr (Branch1 MTranspose t) = (generateNumpyExpr t) ++ ".T" -- caution: might we need parentheses here?
generateNumpyExpr (Branch1 MNegate t) = "-" ++ (generateNumpyExpr t)
generateNumpyExpr (Branch1 MChol t) = "scipy.linalg.cho_factor(" ++ (generateNumpyExpr t) ++ ")"

-----------------------------------------------------
-- Code generation for MATLAB

generateMatlab :: Program -> String
generateMatlab (Seq (x:[])) = generateMatlabStmt x
generateMatlab (Seq (x:xs)) = generateMatlabStmt x ++ "\n" ++ generateMatlab (Seq $ xs)
generateMatlab (Seq []) = ""

generateMatlabStmt :: Stmt -> String
generateMatlabStmt (Assign v e _) = v ++ " = " ++ (generateMatlabExpr e) ++ ";"

generateMatlabExpr :: Expr -> String
generateMatlabExpr (Leaf a) = a
generateMatlabExpr (IdentityLeaf n) = "eye(" ++ (show n) ++ ")"
generateMatlabExpr (Branch3 MTernaryProduct t1 t2 t3) = "(" ++ (generateMatlabExpr t1) ++ " * " ++
                                                        (generateMatlabExpr t2)  ++ " * " ++
                                                        (generateMatlabExpr t3) ++ ")"
generateMatlabExpr (Branch2 MLinSolve t1 t2) = "(" ++ (generateMatlabExpr t1) ++
                                          "\\" ++ (generateMatlabExpr t2)  ++ ")"
generateMatlabExpr (Branch2 MCholSolve t1 t2) = "(" ++ (generateMatlabExpr t1) ++
                                          "\\((" ++ (generateMatlabExpr t1) ++
                                          ")'\\" ++ (generateMatlabExpr t2)  ++ "))"
generateMatlabExpr (Branch2 MProduct t1 t2) = "(" ++ (generateMatlabExpr t1) ++
                                          " * " ++ (generateMatlabExpr t2)  ++ ")"
generateMatlabExpr (Branch2 MSum t1 t2) = "(" ++ (generateMatlabExpr t1) ++ " + " ++ (generateMatlabExpr t2) ++ ")"
generateMatlabExpr (Branch1 MInverse t) = "inv(" ++ (generateMatlabExpr t) ++ ")"
generateMatlabExpr (Branch1 MTranspose t) = (generateMatlabExpr t) ++ "'" -- caution: might we need parentheses here?
generateMatlabExpr (Branch1 MNegate t) = "-" ++ (generateMatlabExpr t)
generateMatlabExpr (Branch1 MChol t) = "chol(" ++ (generateMatlabExpr t) ++ ")"

symTableMatlab :: SymbolTable -> String
symTableMatlab tbl = foldl (++)  "" [tblEntry k m | (k,m) <- Map.toList tbl]  where
  tblEntry k (Matrix n m _) = k ++ " = randn(" ++ (show n) ++ ", " ++ (show m) ++ ");\n"
