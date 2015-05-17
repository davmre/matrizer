module Matrizer.CodeGen where

import qualified Data.Map as Map
import Matrizer.MTypes

-----------------------------------------------------
-- Code generation for numpy arrays

-- atomic_ops=[MTernaryProduct, MLinSolve, MCholSolve, MTriSolve, MColProduct, MProduct, MInverse, MTranspose, MTrace, MDet, MDiagVM, MDiagMV, MEntrySum (MElementWise MLog), (MElementWise MExp)]

-- #precendence = [(MElementWise MReciprocal), MScalarProduct, MNegate,  MSum]

-- parenWrap parentOp MSum s = 
--           | parentOp `elem` atomic_ops = False
--           | (parentOp == MSum) = False
--           | (parentOp == (MElementWise MReciprocal) = True
--           | (parentOp == MScalarProduct) = True
--           | (parentOp == MNegate) = True
-- parenWrap parentOp childOp s = 
--           | parentOp `elem` atomic_ops = False
--           | childOp `elem` atomic_ops = False
--           | otherwise = False
              

--               if atomicParent then False
--               else if atomicChild then False
          

generateNumpy :: Expr -> String
generateNumpy (Leaf a) = a
generateNumpy (IdentityLeaf n) = "np.eye(" ++ (show n) ++ ")"
generateNumpy (ZeroLeaf n m) = "np.zeros((" ++ (show n) ++ ", " ++ (show m) ++ "))"
generateNumpy (LiteralScalar x) = let ix = round x
                                      isInt = x == (fromIntegral ix) in
                                      if isInt then show ix else show x
generateNumpy (Branch3 MTernaryProduct t1 t2 t3) = "np.dot(np.dot(" ++ (generateNumpy t1) ++ 
                                                   ", " ++ (generateNumpy t2)  ++ "), " ++ 
                                                   (generateNumpy t3) ++ ")"
generateNumpy (Branch2 MLinSolve t1 t2) = "np.linalg.solve(" ++ (generateNumpy t1) ++ 
                                          ", " ++ (generateNumpy t2)  ++ ")" 
generateNumpy (Branch2 MTriSolve t1 t2) = "scipy.linalg.solve_triangular(" ++ (generateNumpy t1) ++ 
                                          ", " ++ (generateNumpy t2)  ++ ", lower=True)" 
generateNumpy (Branch2 MCholSolve t1 t2) = "scipy.linalg.cho_solve((" ++ (generateNumpy t1) ++ 
                                          ", True), " ++ (generateNumpy t2)  ++ ")" 
generateNumpy (Branch2 MProduct t1 t2) = "np.dot(" ++ (generateNumpy t1) ++ 
                                          ", " ++ (generateNumpy t2)  ++ ")" 
generateNumpy (Branch2 MScalarProduct t1 t2) = "( " ++ (generateNumpy t1) ++ " * "
                                                  ++ (generateNumpy t2)  ++ ")" 
generateNumpy (Branch2 MColProduct t1 t2) = "( " ++ (generateNumpy t1) ++ ".flatten() * "
                                                  ++ (generateNumpy t2)  ++ ")" 
generateNumpy (Branch2 MDiff t1 t2) = "(" ++ (generateNumpy t1) ++ " - " ++ (generateNumpy t2) ++ ")"
generateNumpy (Branch2 MSum t1 t2) = "(" ++ (generateNumpy t1) ++ " + " ++ (generateNumpy t2) ++ ")"
generateNumpy (Branch2 MHadamardProduct t1 t2) = (generateNumpy t1) ++ " * " ++ (generateNumpy t2)
generateNumpy (Branch1 MInverse t) = "np.linalg.inv(" ++ (generateNumpy t) ++ ")"
generateNumpy (Branch1 MTranspose t) = (generateNumpy t) ++ ".T" -- caution: might we need parentheses here?
generateNumpy (Branch1 (MElementWise MExp) t) = "np.exp(" ++ (generateNumpy t) ++ ")"
generateNumpy (Branch1 (MElementWise MLog) t) = "np.log(" ++ (generateNumpy t) ++ ")"
generateNumpy (Branch1 (MElementWise MReciprocal) t) = "1.0/(" ++ (generateNumpy t) ++ ")"
generateNumpy (Branch1 MChol t) = "np.linalg.cholesky(" ++ (generateNumpy t) ++ ")"
generateNumpy (Branch1 MTrace t) = "np.trace(" ++ (generateNumpy t) ++ ")"
generateNumpy (Branch1 MDet t) = "np.linalg.det(" ++ (generateNumpy t) ++ ")"
generateNumpy (Branch1 MDiagMV t) = "np.diag(" ++ (generateNumpy t) ++ ")"
generateNumpy (Branch1 MDiagVM t) = "np.diag(" ++ (generateNumpy t) ++ ".flatten())"
generateNumpy (Branch1 MEntrySum t) = "np.sum(" ++ (generateNumpy t) ++ ")"

-- we use "empty" let expressions do denote the final quantitity to be computed, but we don't actually
-- need to generate a body for such expressions
generateNumpy (Let lhs rhs tmp (Leaf _)) = lhs ++ " = " ++ (generateNumpy rhs) 
generateNumpy (Let lhs rhs tmp body) = lhs ++ " = " ++ (generateNumpy rhs) ++ "\n" ++ (generateNumpy body)


---------------------------
-- generate a symbol table as a set of random matrices: used for testing

generateTableNumpy :: SymbolTable -> String
generateTableNumpy tbl = foldr (++) "" (map generateMatrixNumpy (Map.toList tbl))

generateMatrixNumpy (v, (Matrix r c props)) = 
                    let randn r c = "np.random.randn(" ++ (show r) ++ ", " ++ (show c) ++ ")" in
                        if PosDef `elem` props
                        then "Ltmp = " ++ (randn r r) ++ "\n" ++ v ++ " = np.dot(Ltmp.T, Ltmp)\n"
                        else if LowerTriangular `elem` props
                        then v ++ "= np.tril(" ++ (randn r c) ++ ")\n"
                        else if Symmetric `elem` props
                        then "Stmp = " ++ (randn r c) ++ "\n" ++ v ++ " = Stmp + Smpt.T\n"
                        else if Diagonal `elem` props
                        then v ++ "= np.diag(np.random.randn(" ++ (show r) ++ "))\n"
                        else v ++ " = " ++ (randn r c) ++ "\n"


---------------------------------------------------------------------------------------------------
-- Code generation for MATLAB

-- WARNING: I don't use MATLAB so most of this code is totally untested/possibly wrong. Use at your own risk (and please contribute patches!). 

generateMatlab :: Expr -> String
generateMatlab (Leaf a) = a
generateMatlab (IdentityLeaf n) = "eye(" ++ (show n) ++ ")"
generateMatlab (ZeroLeaf n m) = "zeros(" ++ (show n) ++ ", " ++ (show m) ++ ")"
generateMatlab (LiteralScalar x) = let ix = round x
                                       isInt = x == (fromIntegral ix) in
                                       if isInt then show ix else show x
generateMatlab (Branch3 MTernaryProduct t1 t2 t3) = "(" ++ (generateMatlab t1) ++ " * " ++
                                                        (generateMatlab t2)  ++ " * " ++
                                                        (generateMatlab t3) ++ ")"
generateMatlab (Branch2 MLinSolve t1 t2) = "(" ++ (generateMatlab t1) ++
                                          "\\" ++ (generateMatlab t2)  ++ ")"
generateMatlab (Branch2 MTriSolve t1 t2) = "(" ++ (generateMatlab t1) ++
                                          "\\" ++ (generateMatlab t2)  ++ ")"
generateMatlab (Branch2 MCholSolve t1 t2) = "(" ++ (generateMatlab t1) ++
                                            "\\((" ++ (generateMatlab t1) ++
                                            ")'\\" ++ (generateMatlab t2) ++ "))"
generateMatlab (Branch2 MProduct t1 t2) = "(" ++ (generateMatlab t1) ++
                                          " * " ++ (generateMatlab t2)  ++ ")"
generateMatlab (Branch2 MScalarProduct t1 t2) = "(" ++ (generateMatlab t1) ++
                                                " * " ++ (generateMatlab t2)  ++ ")"
generateMatlab (Branch2 MColProduct t1 t2) = "bsxfun(@times, " ++ (generateMatlab t1) ++
                                                ", " ++ (generateMatlab t2)  ++ ")"
generateMatlab (Branch2 MDiff t1 t2) = "(" ++ (generateMatlab t1) ++ " - " ++ (generateMatlab t2) ++ ")"
generateMatlab (Branch2 MSum t1 t2) = "(" ++ (generateMatlab t1) ++ " + " ++ (generateMatlab t2) ++ ")"
generateMatlab (Branch2 MHadamardProduct t1 t2) = "(" ++ (generateMatlab t1) ++ " .* " ++ (generateMatlab t2) ++ ")"
generateMatlab (Branch1 MInverse t) = "inv(" ++ (generateMatlab t) ++ ")"
generateMatlab (Branch1 MTranspose t) = (generateMatlab t) ++ "'" -- caution: might need parentheses here?
generateMatlab (Branch1 (MElementWise MExp) t) = "exp(" ++ (generateMatlab t) ++ ")"
generateMatlab (Branch1 (MElementWise MLog) t) = "log(" ++ (generateMatlab t) ++ ")"
generateMatlab (Branch1 (MElementWise MReciprocal) t) = "1.0./(" ++ (generateMatlab t) ++ ")"
generateMatlab (Branch1 MChol t) = "chol(" ++ (generateMatlab t) ++ ")"
generateMatlab (Branch1 MTrace t) = "trace(" ++ (generateMatlab t) ++ ")"
generateMatlab (Branch1 MDet t) = "det(" ++ (generateMatlab t) ++ ")"
generateMatlab (Branch1 MDiagMV t) = "diag(" ++ (generateMatlab t) ++ ")"
generateMatlab (Branch1 MDiagVM t) = "diag(" ++ (generateMatlab t) ++ ")"
generateMatlab (Branch1 MEntrySum t) = "sum(" ++ (generateMatlab t) ++ ")"

generateMatlab (Let lhs rhs tmp (Leaf _)) = lhs ++ " = " ++ (generateMatlab rhs)  ++ "\n"
generateMatlab (Let lhs rhs tmp body) = lhs ++ " = " ++ (generateMatlab rhs) ++ "\n" ++ (generateMatlab body) ++ ";"

symTableMatlab :: SymbolTable -> String
symTableMatlab tbl = foldl (++)  "" [tblEntry k m | (k,m) <- Map.toList tbl]  where
  tblEntry k (Matrix n m _) = k ++ " = randn(" ++ (show n) ++ ", " ++ (show m) ++ ");\n"