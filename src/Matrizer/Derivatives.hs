module Matrizer.Derivatives  (
 differentiate
) where

import Data.List
import qualified Data.Map as Map

import Control.Monad.Error
import Matrizer.MTypes
import Matrizer.RewriteRules
import Matrizer.Optimization
import Matrizer.Analysis
import Matrizer.Search

import Debug.Trace


-- rewrite rules for differentials.
--  inspired by Minka's "Old and New Matrix Algebra Useful for Statistics":
--   https://tminka.github.io/papers/matrix/
-- for an expression y(x), define the differential dy(x) as the part of
--    y(x + dx) - y(x)
-- that is linear in dx. Given an expression for y(x), goal is to manipulate
-- dy(x) until we can directly read off the coefficient for dx.
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
                   _ -> (Branch3 MTernaryProduct (Branch2 MScalarProduct (LiteralScalar (-1)) (Branch1 MInverse a)) d (Branch1 MInverse a))
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


totalDepthHeuristic e _ = -- Right (5 * (sum $ depthHeuristic e) )
                          Right (2 * (sum $ depthHeuristic e)  + ((treeSize e) `quot` 5))
                          -- Right $ 5 * (sum $ depthHeuristic e) + (treeSize e)
tdh tbl e = let Right v = totalDepthHeuristic e tbl in 
                v


astarSucc tbl e = let Right moves = rewriteMoves totalDepthHeuristic optimizationRules tbl (BeamNode e 0 "" Nothing) in
                           [(expr, 1, Just d) | (BeamNode expr _ d _) <- moves]

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
             then Just a
             else Nothing
extractDeriv tbl (Branch1 MTrace (Branch3 MTernaryProduct a b (Branch1 MDifferential _))) = 
             let candidate = (Branch2 MProduct a b) in
             if (sum $ depthHeuristic candidate) == 0
             then Just candidate
             else Nothing

extractDeriv tbl _ = Nothing

goalCheck tbl expr = case extractDeriv tbl expr of
                     (Just d) -> True
                     (Nothing) -> False

runAstar tbl expr = astar expr (astarSucc tbl) (goalCheck tbl) (heuristicCost (tdh tbl)) 100

beamSearch2 :: ScoreFn -> Int -> Int -> Int -> SymbolTable -> Beam -> ThrowsError Beam
beamSearch2 fn 0 _ _ _ beam = return $ beam
beamSearch2 fn iters beamSize nRewrites tbl beam = 
                 do newBeam <- beamIter (rewriteMoves fn optimizationRules) beamSize nRewrites tbl beam []
                    beamSearch2 fn (iters-1) beamSize nRewrites tbl newBeam





decompose :: Expr -> [(Float, Expr)]
decompose (Branch2 MScalarProduct (LiteralScalar c) a) = [(c1*c, d) | (c1, d) <- decompose a]
decompose (Branch2 MSum a b) = (decompose a) ++ (decompose b)
decompose (Branch2 MDiff a b) = (decompose a) ++ [(cb * (-1.0), eb) | (cb, eb) <- (decompose b)]
decompose (Branch2 MProduct a b) = [(ca*cb, (Branch2 MProduct ea eb)) | (ca, ea) <- (decompose a), (cb, eb) <- (decompose b)]
decompose (Branch3 MTernaryProduct a b c) = [(ca*cb*cc, (Branch3 MTernaryProduct ea eb ec)) | (ca, ea) <- (decompose a), (cb, eb) <- (decompose b), (cc, ec) <- (decompose c)]
decompose (Branch1 MTrace a) = [(ca, (Branch1 MTrace ea)) | (ca, ea) <- decompose a]
decompose a = [(1, a)]


factoredAstar :: SymbolTable -> Expr -> Maybe Expr
factoredAstar tbl expr = 
              let exprs = (decompose expr) in
              do derivs <- mapM (derivFromAstar tbl) [expr | (f, expr) <- exprs]
                 let consts = [f | (f, expr) <- exprs ] in
                     return $ recompose $ zip consts derivs
   where recompose ((f, expr):[]) = rescale f expr
         recompose ((f, expr):xs) = (Branch2 MSum (rescale f expr) (recompose xs))
         rescale f expr = if f == 1 then expr else (Branch2 MScalarProduct (LiteralScalar f) expr)


-- convert an expression into another expression representing its
-- derivative wrt a given variable. Apply some trivial optimizations
-- based on linearity, otherwise apply search over differential
-- rewrites.
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
differentiate tbl expr c =   differentiateBySearch tbl expr c


differentiateBySearch tbl expr c = 
          let dd = reduceWithTrace tbl expr c in
          if (sum $ depthHeuristic dd) == 0
          then let Right (Matrix a b props) = treeMatrix (Branch1 (MDeriv c) expr) tbl in
               Just (ZeroLeaf a b)
               else factoredAstar tbl dd

derivFromAstar :: SymbolTable -> Expr -> Maybe Expr
derivFromAstar tbl expr = do r <- runAstar tbl expr
                             extractDeriv tbl (nvalue r)
                           
reduceWithTrace tbl expr c = let d = reduceDifferential c expr 
                                 Right (Matrix d1 d2 dprops) = treeMatrix d tbl in
                                 if (d1==1 && d2==1) then (Branch1 MTrace d) else d


