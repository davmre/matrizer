module Matrizer.Preprocess (
 preprocess,
 makeConcrete
) where

import qualified Data.Map as Map
import Data.List
import Control.Monad.Error

import Matrizer.MTypes
import Matrizer.Derivatives
import Matrizer.Analysis

makeConcrete :: SymbolTable -> Expr -> ThrowsError Expr
makeConcrete tbl (Branch1 (MDeriv v) a) = case differentiate tbl a v of
                                               Just e -> Right e
                                               Nothing -> throwError $ DerivativeFail a v
makeConcrete tbl (Branch1 op e) = do ce <- makeConcrete tbl e
                                     return  (Branch1 op ce)
makeConcrete tbl (Branch2 op a b) = do ca <- makeConcrete tbl a
                                       cb <- makeConcrete tbl b
                                       return (Branch2 op ca cb)
makeConcrete tbl (Branch3 op a b c) = do ca <- makeConcrete tbl a
                                         cb <- makeConcrete tbl b
                                         cc <- makeConcrete tbl c
                                         return (Branch3 op ca cb cc)
makeConcrete tbl (Let lhs rhs tmp body) = do crhs <- (makeConcrete tbl rhs)
                                             cbody <- (makeConcrete tbl body)
                                             return $ Let lhs crhs tmp cbody
makeConcrete tbl e = return e

preprocess :: Expr -> SymbolTable -> ThrowsError Expr
preprocess (Leaf "I") _ = throwError $ AnalysisError "could not infer size of identity matrix"
preprocess (Leaf a) _ = return $ Leaf a
preprocess (IdentityLeaf n) _ = return $ IdentityLeaf n
preprocess (LiteralScalar n) _ = return $ LiteralScalar n
preprocess (Branch1 MDiagMV a) tbl = do newA <- preprocess a tbl
                                        (Matrix n m _) <- treeMatrix newA tbl
                                        if (n ==1 && m > 1) || (n>1 && m==1)
                                        then return $ Branch1 MDiagVM newA
                                        else return $ Branch1 MDiagMV newA
preprocess (Branch1 op a) tbl = do newA <- preprocess a tbl
                                   return $ Branch1 op newA
preprocess (Branch2 op a (Leaf "I")) tbl = 
                do newA <- preprocess a tbl
                   (Matrix n m _) <- typeCheck newA tbl
                   preprocess (Branch2 op newA (IdentityLeaf (idshape2 op True n m))) tbl
preprocess (Branch2 op (Leaf "I") b) tbl = 
                do newB <- preprocess b tbl
                   (Matrix n m _) <- typeCheck newB tbl
                   preprocess (Branch2 op (IdentityLeaf (idshape2 op False n m)) newB) tbl
--preprocess (Branch2 MDiff a b) tbl = 
--                do newA <- preprocess a tbl
--                   newB <- preprocess b tbl
--                   return $ Branch2 MSum newA (Branch2 MScalarProduct (LiteralScalar (-1.0)) newB)
preprocess (Branch2 MProduct a b) tbl = do newA <- preprocess a tbl
                                           newB <- preprocess b tbl
                                           (Matrix n1 m1 _) <- treeMatrix newA tbl
                                           (Matrix n2 m2 _) <- treeMatrix newB tbl
                                           if (n1==1 && m1==1) 
                                           then return $ Branch2 MScalarProduct newA newB
                                           else if (n2==1 && m2==1) 
                                                then return $ Branch2 MScalarProduct newB newA
                                           else if (m1 == 1) && (n1==n2)
                                                then return $ Branch2 MColProduct newA newB
                                           else return $ Branch2 MProduct newA newB
preprocess (Branch2 MLinSolve a b) tbl = do newA <- preprocess a tbl
                                            newB <- preprocess b tbl
                                            (Matrix n1 m1 props) <- treeMatrix newA tbl
                                            if LowerTriangular `elem` props
                                            then return $ Branch2 MTriSolve newA newB
                                            else return $ Branch2 MLinSolve newA newB
preprocess (Branch2 op a b) tbl = do newA <- preprocess a tbl
                                     newB <- preprocess b tbl
                                     return $ Branch2 op newA newB
preprocess (Branch3 _ _ _ _) _ = throwError $ AnalysisError "encountered a ternop while parsing identity matrices, but the parser should never produce ternops!"
preprocess (Let lhs rhs tmp body) tbl  = do newRHS <- preprocess rhs tbl
                                            letMatrix <- treeMatrix newRHS tbl
                                            let newtbl = Map.insert lhs letMatrix tbl
                                            newBody <- preprocess body newtbl
                                            return $ Let lhs newRHS tmp newBody
