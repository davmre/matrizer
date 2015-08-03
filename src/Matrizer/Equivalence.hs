module Matrizer.Equivalence (
  testEquivalence
) where

import Data.List
import qualified Data.Set as Set
import qualified Data.MultiMap as MultiMap
import qualified Data.Map as Map

import Control.Monad.Error
import Matrizer.MTypes
import Matrizer.RewriteRules
import Matrizer.Optimization
import Matrizer.Analysis
import Matrizer.Search

import Matrizer.Derivatives

import Debug.Trace

-- heuristic: 
    -- |subtrees in e2 not present in e1| + |subtrees in e1 not present in e2|
    -- this is 0 at the goal.
    -- it is *not* admissible: a single move can change lots of subtrees. 

equivHeuristic e1 e2 = let map1 = buildSubexpressionMap MultiMap.empty (e1, [])
                           map2 = buildSubexpressionMap MultiMap.empty (e2, []) in
                           symmetricKeyDifference map1 map2

symmetricKeyDifference map1 map2 = 
   let allkeys = Set.toList $ Set.fromList (MultiMap.keys map1 ++ MultiMap.keys map2)
       counts1 = [length $ MultiMap.lookup k map1 | k <- allkeys]
       counts2 = [length $ MultiMap.lookup k map2 | k <- allkeys] in
       sum [abs $ c1 - c2 | (c1, c2) <- zip counts1 counts2]

equivGoal e1 e2 = e1==e2



astarSucc tbl extraRules v  = 
     let Right moves = rewriteMoves (\a b -> Right 1) (optimizationRules ++ extraRules) tbl v in
         [(expr, 1) | (expr, c) <- moves]


-- return whether two expressions are equivalent, or Nothing if
-- could not determine. We need the symbol table since matrix properties
-- can be relevant to equivalence, e.g. A' === A if A is symmetric. 
testEquivalence :: SymbolTable -> Expr -> Expr -> Maybe Bool
testEquivalence tbl e1 e2 = 
                let Right (Matrix r1 c1 _) = treeMatrix e1 tbl 
                    Right (Matrix r2 c2 _) = treeMatrix e2 tbl
                    forward = astar e1 (astarSucc tbl (renameTmpRules tbl e2  )) (equivGoal e2) (heuristicCost (equivHeuristic e2)) 10
                    backward = astar e2 (astarSucc tbl (renameTmpRules tbl e1 )) (equivGoal e1) (heuristicCost (equivHeuristic e1)) 10 in
                if (r1 /= r2 || c1 /= c2) 
                then Just False
                else case (forward, backward) of
                     (Just _, _) -> Just True
                     (_, Just _) -> Just True
                     (Nothing, Nothing) -> Nothing
                


------------------------------------------------------------
-- auxiliary rewrite rule generator for variable name changes.
-- given a target expression e1, extract all the temp variables defined inside of e1, and 
-- for each one return a rewrite rule that changes a tmp variable name to the target name,
-- as long as the variables refer to identically-sized matrices. 
-- this allows proving equivalence of expressions that use different names for tmp variables. 

renameTmpRules :: SymbolTable -> Expr -> Rules
renameTmpRules tbl1 e1 = [renameTmpRule v m | (v, m) <- tmpMatrices tbl1 e1]
  where renameTmpRule v (Matrix r1 c1 _) tbl (Let lhs rhs True body) = 
          let Right (Matrix r2 c2 _) = treeMatrix rhs tbl in
              if (r1==r2 && c1==c2) then Just $ Let v rhs True (recursiveRename lhs v body)
              else Nothing
        renameTmpRule _ _ _ _ = Nothing
        recursiveRename old new (Leaf a) = if a==old then (Leaf new) else (Leaf a)
        recursiveRename old new (Branch1 op a) = Branch1 op (recursiveRename old new a)
        recursiveRename old new (Branch2 op a b) = Branch2 op (recursiveRename old new a) (recursiveRename old new b)
        recursiveRename old new (Branch3 op a b c) = Branch3 op (recursiveRename old new a) (recursiveRename old new b) (recursiveRename old new c)
        recursiveRename old new (Let lhs rhs tmp body) = Let lhs (recursiveRename old new rhs) tmp (recursiveRename old new body)
        recursiveRename old new a = a

tmpMatrices :: SymbolTable -> Expr -> [(VarName, Matrix)]
tmpMatrices tbl a@(Let lhs rhs True body) = 
               let Right m = treeMatrix rhs tbl 
                   Right newTbl = tblBind a tbl in
                   (lhs, m):(tmpMatrices newTbl body)
tmpMatrices tbl a@(Let lhs rhs False body) = 
               let Right newTbl = tblBind a tbl in
                   tmpMatrices newTbl body
tmpMatrices _ _ = []

-------------------------------------------------

--- derivative examples
llSymbols = Map.fromList [("X", Matrix 100 100 []), ("A", Matrix 100 100 []), ("B", Matrix 100 100 []), ("C", Matrix 100 100 [])]
inp = (Branch1 MTrace (Branch3 MTernaryProduct (Branch2 MProduct (Leaf "A") (Branch1 MTranspose (Leaf "X"))) (Leaf "B") (Branch2 MProduct (Leaf "X") (Leaf "C"))) )
Just inp_deriv = differentiate llSymbols inp "X"
target = (Branch2 MSum 
            (Branch2 MProduct 
               (Branch2 MProduct 
                  (Branch2 MProduct (Leaf "C") (Leaf "A") ) 
                  (Branch1 MTranspose (Leaf "X"))) 
               (Leaf "B")  ) 
            (Branch2 MProduct 
               (Branch2 MProduct 
                  (Branch2 MProduct (Branch1 MTranspose (Leaf "A")) (Branch1 MTranspose (Leaf "C")) ) 
                  (Branch1 MTranspose (Leaf "X"))) 
               (Branch1 MTranspose (Leaf "B")  ) ))