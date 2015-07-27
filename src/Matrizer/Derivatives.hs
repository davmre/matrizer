module Matrizer.Derivatives  (
 differentiate
) where

import Data.List
import qualified Data.Map as Map

import Control.Monad.Error
import Matrizer.MTypes
import Matrizer.Optimization
import Matrizer.Analysis
import Matrizer.Search

llexpr1 = (Branch3 MTernaryProduct (Branch1 MTranspose (Leaf "w")) (Branch2 MProduct (Branch1 MTranspose (Leaf "X")) (Leaf "X") ) (Leaf "w"))
llexpr2 = (Branch2 MScalarProduct (LiteralScalar 2) (Branch2 MProduct (Branch1 MTranspose (Leaf "w")) (Branch2 MProduct (Branch1 MTranspose (Leaf "X")) (Leaf "y"))))
llexpr3 = (Branch2 MProduct (Branch1 MTranspose (Leaf "y")) (Leaf "y"))
llexpr = (Branch2 MSum (Branch2 MDiff llexpr1 llexpr2 ) llexpr3)

llSymbols :: SymbolTable
llSymbols = Map.fromList [("X", Matrix 100 2 []), ("y", Matrix 100 1 []), ("w", Matrix 2 1 [])]

llX = reduceDifferential "X" (Branch1 MTrace llexpr)


reduceDifferential :: VarName -> Expr -> Expr
reduceDifferential v (Leaf a) = if a==v 
                                    then Branch1 MDifferential (Leaf a)
                                    else (ZeroLeaf 1 1)
reduceDifferential v (IdentityLeaf n) = ZeroLeaf n n
reduceDifferential v (ZeroLeaf m n) = ZeroLeaf m n
reduceDifferential v (LiteralScalar a) = ZeroLeaf 1 1
reduceDifferential v (Branch2 MProduct t1 t2) = 
                   let d1 = reduceDifferential v t1
                       d2 = reduceDifferential v t2 in
                   case (d1, d2) of
                   ( (ZeroLeaf _ _), (ZeroLeaf _ _)) -> d1
                   ( (ZeroLeaf _ _), _) -> (Branch2 MProduct t1 d2)
                   ( _, (ZeroLeaf _ _)) -> (Branch2 MProduct d1 t2)
                   ( _, _) -> (Branch2 MSum (Branch2 MProduct d1 t2) (Branch2 MProduct t1 d2))
reduceDifferential v (Branch3 MTernaryProduct t1 t2 t3) = reduceDifferential v (Branch2 MProduct t1 (Branch2 MProduct t2 t3))
reduceDifferential v (Branch2 MSum a b) = 
                   let d1 = (reduceDifferential v a)
                       d2 = (reduceDifferential v b) in
                    case (d1, d2) of
                    ( _, (ZeroLeaf _ _)) -> d1
                    ( (ZeroLeaf _ _), _) -> d2
                    ( _, _) -> (Branch2 MSum d1 d2)
reduceDifferential v (Branch2 MDiff a b) = 
                   let d1 = (reduceDifferential v a)
                       d2 = (reduceDifferential v b) in
                    case (d1, d2) of
                    ( _, (ZeroLeaf _ _)) -> d1
                    ( (ZeroLeaf _ _), _) -> (Branch2 MScalarProduct (LiteralScalar (-1)) d2)
                    ( _, _) -> (Branch2 MDiff d1 d2)
reduceDifferential v (Branch2 MScalarProduct a b) = 
                   let d = (reduceDifferential v b) in
                   case d of
                   (ZeroLeaf _ _) -> d
                   _ -> (Branch2 MScalarProduct a d)
reduceDifferential v (Branch2 MHadamardProduct t1 t2) = Branch2 MSum (Branch2 MHadamardProduct (reduceDifferential v t1) t2) (Branch2 MHadamardProduct t1 (reduceDifferential v t2))
reduceDifferential v (Branch1 MTrace a) = 
                   let d = (reduceDifferential v a) in
                   case d of
                   (ZeroLeaf _ _) -> (ZeroLeaf 1 1)                           
                   _ -> Branch1 MTrace d
reduceDifferential v (Branch1 MInverse a) = 
                   let d = (reduceDifferential v a) in
                   case d of
                   (ZeroLeaf _ _) -> (ZeroLeaf 1 1)
                   _ -> (Branch3 MTernaryProduct (Branch1 MInverse a) d (Branch1 MInverse a))
reduceDifferential v (Branch1 MDet a) = 
                   let d = (reduceDifferential v a) in
                   case d of
                   (ZeroLeaf _ _) -> (ZeroLeaf 1 1)
                   _ -> Branch2 MScalarProduct (Branch1 MDet a) (Branch1 MTrace (Branch2 MProduct (Branch1 MInverse a) d))
reduceDifferential v (Branch1 (MElementWise MLog) (Branch1 MDet a)) = Branch1 MTrace (Branch2 MProduct (Branch1 MInverse a) (reduceDifferential v a))
reduceDifferential v (Branch1 MTranspose a) = 
                   let d = (reduceDifferential v a) in
                   case d of
                   (ZeroLeaf a b) -> (ZeroLeaf b a)
                   _ -> Branch1 MTranspose d


incrList :: [Int] -> [Int]
incrList l = [n+1 | n <- l]

depthHeuristic :: Expr -> [Int]
depthHeuristic (Leaf _) = []
depthHeuristic (IdentityLeaf _) = []
depthHeuristic (ZeroLeaf _ _) = []
depthHeuristic (LiteralScalar _) = []
depthHeuristic (Branch1 MDifferential _) = [1]
depthHeuristic (Branch1 _ a) = incrList $ depthHeuristic a
depthHeuristic (Branch2 _ a b) = let l1 = depthHeuristic a
                                     l2 = depthHeuristic b in
                                 (incrList l1) ++ (incrList l2)
depthHeuristic (Branch3 _ a b c) = let l1 = depthHeuristic a
                                       l2 = depthHeuristic b
                                       l3 = depthHeuristic c in
                                   (incrList l1) ++ (incrList l2) ++ (incrList l3)
treeSize :: Expr -> Int
treeSize (Leaf _) = 1
treeSize (IdentityLeaf _) = 1
treeSize (ZeroLeaf _ _) = 1
treeSize (LiteralScalar _) = 1
treeSize (Branch1 MDifferential _) = 1
treeSize (Branch1 _ a) = 1 + treeSize a
treeSize (Branch2 _ a b) = let l1 = treeSize a
                               l2 = treeSize b in
                               l1 + l2 + 1
treeSize (Branch3 _ a b c) = let l1 = treeSize a
                                 l2 = treeSize b
                                 l3 = treeSize c in
                                 l1 + l2 + l3

totalDepthHeuristic e _ = Right $ 5 * (sum $ depthHeuristic e) + (treeSize e)
tdh tbl e = let Right v = totalDepthHeuristic e tbl in 
                v

astarSucc tbl v = let Right moves = rewriteMoves totalDepthHeuristic tbl v in
                      [(expr, 1) | (expr, c) <- moves]

extractDeriv :: SymbolTable ->  Expr -> Maybe Expr
extractDeriv tbl (Branch1 MDifferential k) = 
             let Right (Matrix r c _) = treeMatrix k tbl in 
             Just (IdentityLeaf r)
extractDeriv tbl (Branch2 MProduct a (Branch1 MDifferential _)) = if (sum $ depthHeuristic a) == 0
                                                                  then Just a
                                                                  else Nothing
extractDeriv tbl (Branch3 MTernaryProduct a b (Branch1 MDifferential _)) = 
             let candidate = (Branch2 MProduct a b) in
             if (sum $ depthHeuristic candidate) == 0
             then Just candidate
             else Nothing
extractDeriv tbl (Branch2 MScalarProduct a (Branch1 MDifferential _)) = 
             if (sum $ depthHeuristic a) == 0
             then Just a
             else Nothing
extractDeriv tbl (Branch1 MTrace (Branch2 MProduct a (Branch1 MDifferential _))) = 
             if (sum $ depthHeuristic a) == 0
             then case a of
             (Branch1 MTranspose ta) -> Just ta
             _ -> Just (Branch1 MTranspose a)
             else Nothing
extractDeriv tbl (Branch1 MTrace (Branch3 MTernaryProduct a b (Branch1 MDifferential _))) = 
             let candidate = (Branch2 MProduct a b) in
             if (sum $ depthHeuristic candidate) == 0
             then Just (Branch1 MTranspose candidate)
             else Nothing

extractDeriv tbl _ = Nothing

goalCheck tbl expr = case extractDeriv tbl expr of
                     (Just d) -> True
                     (Nothing) -> False

runAstar tbl expr = astar expr (astarSucc tbl) (goalCheck tbl) (heuristicCost (tdh tbl))

beamSearch2 :: ScoreFn -> Int -> Int -> Int -> SymbolTable -> Beam -> ThrowsError Beam
beamSearch2 fn 0 _ _ _ beam = return $ beam
beamSearch2 fn iters beamSize nRewrites tbl beam = 
                 do newBeam <- beamIter (rewriteMoves fn) beamSize nRewrites tbl beam
                    beamSearch2 fn (iters-1) beamSize nRewrites tbl newBeam


llSymbols2 :: SymbolTable
llSymbols2 = Map.fromList [("X", Matrix 100 100 []), ("A", Matrix 100 100 []),("B", Matrix 100 100 [])]

-- llexpr_trivial = (Branch1 MTrace (Branch3 MTernaryProduct (Leaf "A")  (Branch1 MTranspose (Leaf "X")) (Leaf "B")))
llexpr_trivial = (Branch1 MTrace (Branch2 MProduct  (Leaf "X") (Leaf "X")))


differentiate :: SymbolTable -> Expr -> VarName -> Maybe Expr
differentiate tbl (Branch2 MSum a b) c = do da <- differentiate tbl a c
                                            db <- differentiate tbl b c
                                            return $ (Branch2 MSum da db)
differentiate tbl (Branch2 MDiff a b) c = do da <- differentiate tbl a c
                                             db <- differentiate tbl b c
                                             return $ (Branch2 MDiff da db)
differentiate tbl (Branch2 MScalarProduct (LiteralScalar s) a) c = 
              do da <- differentiate tbl a c
                 return $ (Branch2 MScalarProduct (LiteralScalar s) da)
differentiate tbl expr c = let dd = reduceDifferential c expr in
                           if (sum $ depthHeuristic dd) == 0
                           then let Right (Matrix a b props) = treeMatrix (Branch1 (MDeriv c) expr) tbl in
                                Just (ZeroLeaf a b)
                           else do r <- runAstar tbl dd 
                                   extractDeriv tbl (nvalue r)
                           



-- collectDifferential tbl v Expr -> Maybe Expr
-- collectDifferential tbl v (Branch1 MDifferential (Leaf a)) = 
--            do (Matrix r1 c1 _) <- (Map.lookup a tbl)           
--               if a==v then Just (IdentityLeaf r1)
--               else Just (ZeroLeaf r1 c1)
-- collectDifferential tbl v (Leaf a) = Nothing
-- collectDifferential tbl v (IdentityLeaf n) = Nothing
-- collectDifferential tbl v (ZeroLeaf n) = Nothing
-- collectDifferential tbl v (LiteralScalar a) = Nothing
-- collectDifferential tbl v (Branch2 MProduct a (Branch1 MDifferential (Leaf b))) = if v==b then Just a

-- differentiate SymbolTable -> Expr -> Expr
-- differentiate tbl (Branch1 (MDeriv v) e) = (Branch1 (MUnresolvedDeriv v) (MDifferential (differentiate tbl e)))



-- so we start with a (deriv v e), from which we should already be
-- able to compute an output dimension.  then we can run reduceDifferential
-- which gives back an expression in which the only derivs are
-- elemental dX.-- In most cases we just want the coefficients of the
-- dXs. One way to do this is to write another function that accepts a
-- reduced expression and bubbles up the sum of the coefficients on
-- differential terms. Another way is to modify reduceDifferential to do this
-- directly.