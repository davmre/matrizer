module Matrizer.Optimization where

import qualified Data.Map as Map
import qualified Data.MultiMap as MultiMap
import qualified Data.Set as Set
import Data.List
import Data.Ord

import Control.Monad.Error

import Matrizer.MTypes
import Matrizer.Analysis
import Matrizer.RewriteRules
import Debug.Trace

----------------------------------

-- convenience
mkUniq = Set.toList . Set.fromList

--------------------------------------------------------------------------------------------------------
-- Zipper definitions for the Expr structure (see http://learnyouahaskell.com/zippers)

data Crumb = SingleCrumb UnOp
           | LeftCrumb BinOp Expr
           | RightCrumb BinOp Expr
           | TernCrumbL TernOp Expr Expr
           | TernCrumbC TernOp Expr Expr
           | TernCrumbR TernOp Expr Expr
           | RhsCrumb VarName Bool Expr
           | BodyCrumb VarName Expr Bool
           deriving (Show, Eq)
type Breadcrumbs = [Crumb]

type MZipper = (Expr, Breadcrumbs)

goLeft :: MZipper -> ThrowsError MZipper
goLeft (Let lhs rhs tmp body, bs) = Right (rhs, RhsCrumb lhs tmp body : bs)
goLeft (Branch2 op l r, bs) = Right (l, LeftCrumb op r:bs)
goLeft (Branch3 op l c r, bs) = Right (l, TernCrumbL op c r : bs)
goLeft (n, bs) = throwError $ BadCrumbs n ("going left: " ++ show bs)

goRight :: MZipper -> ThrowsError MZipper
goRight (Let lhs rhs tmp body, bs) = Right (body, BodyCrumb lhs rhs tmp : bs)
goRight (Branch2 op l r, bs) = Right (r, RightCrumb op l:bs)
goRight (Branch3 op l c r, bs) = Right (r, TernCrumbR op l c : bs)
goRight (n, bs) = throwError $ BadCrumbs n ("going right: " ++ show bs)

goDown :: MZipper -> ThrowsError MZipper
goDown (Branch1 op t, bs) = Right (t, SingleCrumb op : bs)
goDown (Branch3 op l c r, bs) = Right (c, TernCrumbC op l r : bs)
goDown (n, bs) = throwError $ BadCrumbs n ("going down: " ++ show bs)

goUp :: MZipper -> ThrowsError MZipper
goUp (t, SingleCrumb op : bs) = Right (Branch1 op t, bs)
goUp (t, LeftCrumb op r : bs) = Right (Branch2 op t r, bs)
goUp (t, RightCrumb op l : bs) = Right (Branch2 op l t, bs)
goUp (t, RhsCrumb lhs tmp body : bs) = Right (Let lhs t tmp body, bs)
goUp (t, BodyCrumb lhs rhs tmp : bs) = Right (Let lhs rhs tmp t, bs)
goUp (t, TernCrumbL op c r : bs) = Right (Branch3 op t c r , bs)
goUp (t, TernCrumbC op l r : bs) = Right (Branch3 op l t r , bs)
goUp (t, TernCrumbR op l c : bs) = Right (Branch3 op l c t , bs)
goUp (n, bs) = throwError $ BadCrumbs n ("going up: " ++ show bs)

topMost :: MZipper -> MZipper
topMost (t,[]) = (t,[])
topMost z = topMost $ trapError z id (goUp z)


zipperToTree :: MZipper -> Expr
zipperToTree (n, _) = n


-- Given a set of breadcrumbs pointing to some location in an old expression tree,
-- and a zipper initialized to the root of a new expression tree,
-- return the zipper pointing to the corresponding location in the new
-- tree. 
-- Note that the breadcrumbs need to be in the reverse order of a
-- standard zipper, i.e. they need to give directions from the top
-- down, rather than the bottom up. Currently I solve this by just
-- reversing the list. A better approach would be to define a new set
-- of goLeft/Right/etc methods that build the list in the reverse
-- order, and use these in buildSubexpressionMap -- defining new
-- methods would also let us just store a list of crumbs while
-- ignoring the zipper aspect entirely (so we're not passing around
-- pieces of the old trees which we then ignore). I haven't done this
-- yet because it's messy, and it's not clear this is a bottleneck.
recreateZipper :: Breadcrumbs -> ThrowsError MZipper -> ThrowsError MZipper
recreateZipper (LeftCrumb _ _:bs) (Right z) = recreateZipper bs (goLeft z)
recreateZipper ((RightCrumb _ _):bs) (Right z) = recreateZipper bs (goRight z)
recreateZipper ((SingleCrumb _):bs) (Right z) = recreateZipper bs (goDown z)
recreateZipper ((TernCrumbL _ _ _):bs) (Right z) = recreateZipper bs (goLeft z)
recreateZipper ((TernCrumbC _ _ _):bs) (Right z) = recreateZipper bs (goDown z)
recreateZipper ((TernCrumbR _ _ _):bs) (Right z) = recreateZipper bs (goRight z)
recreateZipper ((RhsCrumb _ _ _):bs) (Right z) = recreateZipper bs (goLeft z)
recreateZipper ((BodyCrumb _ _ _):bs) (Right z) = recreateZipper bs (goRight z)
recreateZipper [] z = z 
recreateZipper _ z = z 


-----------------------------------------------------------------
-- Main optimizer logic

optimize :: Expr -> SymbolTable -> ThrowsError BeamNode
optimize expr tbl = beamSearchWrapper treeFLOPs 10 20 2 tbl expr



----------------------------------------------------------------
type ScoreFn = (Expr -> SymbolTable -> ThrowsError Int)

-- (expr score move length
data BeamNode = BeamNode Expr Int String (Maybe BeamNode) deriving (Eq, Show, Ord)
type Beam = [BeamNode]

-- compare two beamNodes ignoring their histories
beamNodeEq (BeamNode e1 s1 _ _) (BeamNode e2 s2 _ _) =  (s1 == s2) && (e1==e2)

-- output a beamNode history as readable text
pprintOptPath n = let (s, k) = pprintOptPathHelper (Just n) in
                      s
 where pprintOptPathHelper Nothing = ("", 0)
       pprintOptPathHelper (Just (BeamNode e s d Nothing)) = ("", 1)
       pprintOptPathHelper (Just (BeamNode e s d n)) = 
        let (prevS, k) = pprintOptPathHelper n 
            newS = prevS ++ (show k) ++ ": applied " ++ d ++ ", score " ++ (show s) ++ "\n" ++ (pprint e) ++ "\n" in
            (newS, k+1)               


---------------------------------------------------------------

beamSearchWrapper fn iters beamSize nRewrites tbl expr = 
                  do initScore <- fn  expr tbl
                     beam <- beamSearch fn optimizationRules iters beamSize nRewrites tbl [(BeamNode expr initScore "" Nothing)] []
                     return $ head beam

-- recursive wrapper function to iterate beam search
beamSearch :: ScoreFn -> Rules -> Int -> Int -> Int -> SymbolTable -> Beam -> Beam -> ThrowsError Beam
beamSearch fn rules 0 _ _ _ beam prevBeam = return $ beam
beamSearch fn rules iters beamSize nRewrites tbl beam prevBeam = 
                 do newBeam1 <- beamIter (commonSubexpMoves fn) beamSize 1 tbl beam []
                    newBeam2 <- beamIter (rewriteMoves fn rules) beamSize nRewrites tbl newBeam1 prevBeam
                    beamSearch fn rules (iters-1) beamSize nRewrites tbl newBeam2 newBeam1
                    
beamSearchDebug :: ScoreFn -> Rules -> Int -> Int -> Int -> SymbolTable -> Beam -> ThrowsError ([Beam])
beamSearchDebug fn rules 0 _ _ _ beam = return (beam:[])
beamSearchDebug fn rules iters beamSize nRewrites tbl beam = 
                 do newBeam1 <- beamIter (commonSubexpMoves fn) beamSize 1 tbl beam []
                    newBeam2 <- beamIter (rewriteMoves fn rules) beamSize nRewrites tbl newBeam1 []
                    beamlog <- beamSearchDebug fn rules (iters-1) beamSize nRewrites tbl newBeam2
                    return $ newBeam1 : newBeam2 : beamlog
           

type MoveRewriter = SymbolTable -> BeamNode -> ThrowsError Beam

-- single iteration of beam search: compute all rewrites and take the best few
beamIter :: MoveRewriter -> Int -> Int -> SymbolTable -> Beam -> Beam -> ThrowsError Beam
beamIter rw beamSize nRewrites tbl oldBeam prevBeam = 
   let prevExps = [e | (BeamNode e i _ _) <- prevBeam]
       novel = [ BeamNode e i s b | (BeamNode e i s b) <- oldBeam, not $ elem e prevExps]
       ignored = [BeamNode e i s b | (BeamNode e i s b)  <- oldBeam, elem e prevExps] in
      do rewrites <- reOptimize nRewrites rw tbl novel
         return $ take beamSize (sortBy (comparing beamScore) (ignored ++ rewrites))
   where beamScore (BeamNode _ s _ _) = s

-- for cse, same pattern of generating a list of rewrites and sorting them
-- 'reoptimize n' takes a symboltable and a list of candidates, and generates a new list
-- reoptimizeOnce is just reOptimize N=1, minus deduping
-- optimizerTraversal takes a table and a breadCrumb, and generates a list of (expr,scores). 



-- generate all scored rewrites accessible by applying at most n rewrite rules
reOptimize :: Int -> MoveRewriter -> SymbolTable -> Beam -> ThrowsError Beam
reOptimize 0 rw tbl candidates = return $ candidates
reOptimize n rw tbl candidates = 
           do iter1 <- reOptimizeOnce rw tbl candidates
              reOptimize (n-1) rw tbl (nubBy beamNodeEq (candidates ++ iter1)) 


-- generate all rewrites of the given scored list of expressions
reOptimizeOnce :: MoveRewriter -> SymbolTable -> Beam -> ThrowsError Beam
reOptimizeOnce _ _ [] = return $ []
reOptimizeOnce rw tbl (node@(BeamNode _ score _ _):ts) = 
  do rewrites <- rw tbl node
     newTs <- reOptimizeOnce rw tbl ts
     return $ rewrites ++ newTs


----------------------------------------------------------------

-- clean up tmp variables:
--  - first split the program into lists of tmp and non-tmp statements
--  - for each non-tmp statement, make a list of all the

variablesUsed :: Expr -> [VarName] -> [VarName]
variablesUsed (Leaf a) vs = a:vs
variablesUsed (ZeroLeaf _ _) vs = vs
variablesUsed (IdentityLeaf _ ) vs = vs
variablesUsed (LiteralScalar _ ) vs = vs
variablesUsed (Branch1 _ a ) vs = variablesUsed a vs
variablesUsed (Branch2 _ a b) vs = variablesUsed b (variablesUsed a vs)
variablesUsed (Branch3 _ a b c) vs = variablesUsed c (variablesUsed b (variablesUsed a vs))
variablesUsed (Let lhs rhs tmp body) vs = variablesUsed body (variablesUsed rhs vs)

cleanTmp :: Expr -> Expr
cleanTmp prgm = let vs = variablesUsed prgm [] in
                    removeUnused prgm vs
                where 
                removeUnused (Let lhs rhs False body) used = Let lhs rhs False (removeUnused body used)
                removeUnused (Let lhs rhs True body) used = if lhs `elem` used
                                                             then (Let lhs rhs True (removeUnused body used)) 
                                                             else removeUnused body used
                removeUnused e _  = e
                    
---------------------------------------------------------------

-- common subexpression elimination code
                
-- a subexpression map is a map from subexpressions to (lists of) locations
-- where each subexpression occurs.
type SubExprMap = MultiMap.MultiMap Expr Breadcrumbs

-- build a subexpression map for a given expression
buildSubexpressionMap :: SubExprMap  -> MZipper -> SubExprMap
buildSubexpressionMap smap z@( n@(Branch1 _ _), bs) = 
                      let childMap = trapError smap (buildSubexpressionMap smap) (goDown z) in
                      MultiMap.insert n bs childMap 
buildSubexpressionMap smap z@( n@(Branch2 _ _ _), bs) = 
                      let leftMap = trapError smap (buildSubexpressionMap smap) (goLeft z)
                          rightMap = trapError leftMap (buildSubexpressionMap leftMap) (goRight z) in
                      MultiMap.insert n bs rightMap 
buildSubexpressionMap smap z@( n@(Let _ _ _ _), bs) = 
                      let leftMap = trapError smap (buildSubexpressionMap smap) (goLeft z)
                          rightMap = trapError leftMap (buildSubexpressionMap leftMap) (goRight z) in
                      rightMap  -- don't actually include the 'let' itself as a subexpression that can
                                -- be factored out
buildSubexpressionMap smap z@( n@(Branch3 _ _ _ _), bs) = 
                      let leftMap = trapError smap (buildSubexpressionMap smap) (goLeft z)
                          centerMap = trapError leftMap (buildSubexpressionMap leftMap) (goDown z)
                          rightMap = trapError centerMap (buildSubexpressionMap centerMap) (goRight z) in
                      MultiMap.insert n bs rightMap 
buildSubexpressionMap smap (Leaf _ , _) = smap
buildSubexpressionMap smap (n@(IdentityLeaf _), bs) = smap
buildSubexpressionMap smap (n@(ZeroLeaf _ _), bs) = smap
buildSubexpressionMap smap (n@(LiteralScalar _), bs) = smap


-- generate a list of all common subexpressions: expressions that occur at least twice
commonSubExpressions :: SubExprMap -> [(Expr, [Breadcrumbs])]
commonSubExpressions smap = let l = Map.toList $ MultiMap.toMap smap
                                l_notrans = filter (\(e, _) -> not $ isTranspose e) l in
                            filter (\(_, locs) -> (length locs > 1)) l_notrans
                            where isTranspose (Branch1 MTranspose (Leaf _)) = True
                                  isTranspose _ = False

-- return the first name of the form "tmp1", "tmp2", etc. not already in use. 
chooseTmpVarname :: Expr -> VarName
chooseTmpVarname e = let varsUsed = mkUniq (variablesUsed e [])
                         tmps = ["tmp" ++ (show n) | n <- [1..] ]
                         unusedTmps = filter (not . ((flip elem) varsUsed)) tmps in
                     head unusedTmps


-- given an expression, a set of breadcrumbs leading to a subexpression, and a new variable name,
-- return the expression in which the subexpression is replaced by a leaf node with the given
-- variable name.
replaceSubExp :: Expr -> Breadcrumbs -> VarName -> ThrowsError Expr
replaceSubExp e bs newVar = do newZipper <- recreateZipper (reverse bs) (Right (e, []))
                               return $ reconstructTree newZipper (Leaf newVar)

-- iterate the previous function
replaceSubExprs :: Expr -> [Breadcrumbs] -> VarName -> ThrowsError Expr
replaceSubExprs e [] _ = return $ e
replaceSubExprs e (bs:bss) newVar = do replaced <- replaceSubExp e bs newVar
                                       replaceSubExprs replaced bss newVar


-- check whether all required variables for the subexpression are defined in
-- the current symbol table
tblMeetsDeps deps tbl = and [Map.member d tbl | d <- deps]

-- insert a 'let' expression for the new subexpression in the highest legal place
insertCommonDef :: SymbolTable -> Expr ->  VarName -> Expr -> ThrowsError (SymbolTable, Expr)
insertCommonDef tbl e lhs rhs = let dependents = mkUniq $ variablesUsed rhs [] in
                                do (ntbl, (sube, bs)) <- if tblMeetsDeps dependents tbl then Right (tbl, (e, []))
                                                         else findInsertionPoint dependents tbl (e, [])
                                   return $ (ntbl, reconstructTree (sube, bs) (Let lhs rhs True sube))

-- descend through an expression. whenever we see a 'let' expression, 
-- check to see if all dependencies are now met. if so, the insertion
-- point is just below the 'let'. 
-- HACK: this currently assumes that 'let's  only occur at the
-- top of an expression. I really need to guarantee this in the type system. 
findInsertionPoint :: [VarName] -> SymbolTable -> MZipper -> ThrowsError (SymbolTable, MZipper)
findInsertionPoint deps tbl z@( n@(Let _ _ _ _), _) = 
             do newtbl <- tblBind n tbl
                body <- goRight z
                if tblMeetsDeps deps newtbl then return $ (newtbl, body)
                else findInsertionPoint deps newtbl body


-- High-level function to transform an expression, factoring out a subexpression
-- as identitified by commonSubExpressions
factorSubExpression :: ScoreFn -> BeamNode -> SymbolTable -> (Expr, [Breadcrumbs]) -> ThrowsError BeamNode
factorSubExpression fn node@(BeamNode e oldScore _ _) tbl (subexp, locs) = 
  let tmpVar = chooseTmpVarname e in 
      do factoredOut <- replaceSubExprs e locs tmpVar
         (ntbl, factoredIn) <- insertCommonDef tbl factoredOut tmpVar subexp 
         flops <- fn subexp ntbl 
         let score = (1-(length locs)) * flops + letcost_CONST in
             return $ BeamNode factoredIn (oldScore + score) ("factored out " ++ pprint subexp) (Just node)


-- Given an expression, return all versions reachable by factoring out a single subexpression
commonSubexpMoves :: ScoreFn -> MoveRewriter
commonSubexpMoves fn tbl n@(BeamNode e _ _ _) = 
  let smap = buildSubexpressionMap MultiMap.empty (e, [])
      subexprs = commonSubExpressions smap in
      mapM (factorSubExpression fn n tbl) subexprs
                          


-------------------------------------------------------------------------------------

-- Given a zipper corresponding to a position (node) in a tree, return
-- the list of all new trees constructable by applying a single
-- optimization rule either at the current node, or (recursively) at
-- any descendant node. Note: the transformed trees we return are
-- rooted at the toplevel, i.e. they have been 'unzipped' by
-- reconstructTree.

rewriteMoves :: ScoreFn -> Rules -> MoveRewriter
rewriteMoves fn rules tbl node@(BeamNode e oldScore _ _)  =  
  do scores <- (optimizerTraversal fn tbl rules (e, []))
     return $ map toBeam scores
  where toBeam (e2,s,rule) = BeamNode e2 (oldScore + s) rule (Just node)

optimizerTraversal :: ScoreFn -> SymbolTable -> Rules -> MZipper -> ThrowsError [(Expr, Int, String)]
optimizerTraversal _ _ _ (Leaf _, _) = return $ []
optimizerTraversal _ _ _ (ZeroLeaf _ _, _) = return $ []
optimizerTraversal _ _ _ (IdentityLeaf _, _) = return $ []
optimizerTraversal _ _ _ (LiteralScalar _, _) = return $ []
optimizerTraversal fn tbl rules z@( n@(Branch3 _ _ _ _), _) = 
                   do thisNode <- optimizeAtNode fn tbl rules n
                      leftBranch <- goLeft z >>= optimizerTraversal fn tbl rules
                      centerBranch <- goDown z >>= optimizerTraversal fn tbl rules
                      rightBranch <- goRight z >>= optimizerTraversal fn tbl rules
                      return $ (map (reconstructTreeScore z) thisNode) ++ leftBranch ++ centerBranch ++ rightBranch
optimizerTraversal fn tbl rules z@( n@(Branch2 _ _ _), _) =
                   do thisNode <- optimizeAtNode fn tbl rules n
                      leftBranch <- goLeft z >>= optimizerTraversal fn tbl rules
                      rightBranch <- goRight z >>= optimizerTraversal fn tbl rules
                      return $ (map (reconstructTreeScore z) thisNode) ++ leftBranch ++ rightBranch
optimizerTraversal fn tbl rules z@( n@(Let v a _ _), _) =
                   do thisNode <- optimizeAtNode fn tbl rules n
                      leftBranch <- goLeft z >>= optimizerTraversal fn tbl rules
                      boundTbl <- tblBind n tbl
                      rightBranch <- goRight z >>= optimizerTraversal fn boundTbl (("recognize existing variable " ++ v, recognizeVar v a) : rules)
                      return $ (map (reconstructTreeScore z) thisNode) ++ leftBranch ++ rightBranch
optimizerTraversal fn tbl rules z@( n@(Branch1 _ _), _) =
                   do thisNode <- optimizeAtNode fn tbl rules n
                      descendants <- goDown z >>= optimizerTraversal fn tbl rules
                      return $ (map (reconstructTreeScore z) thisNode) ++ descendants


-- Given a tree node, return a list of all transformed nodes that can
-- be generated by applying optimization rules at that node, along with
-- the net change in FLOPs, and the rule applied
optimizeAtNode :: ScoreFn -> SymbolTable -> Rules -> Expr -> ThrowsError [(Expr, Int, String)]
optimizeAtNode fn tbl rules t = let opts = mapMaybeFunc t [wwrap rule (f tbl) | (rule, f) <- rules ] in
                                 scoreOptimizations fn tbl t opts
 
wwrap :: String -> (Expr -> Maybe Expr) -> Expr -> Maybe (Expr, String)
wwrap rule f e = do e1 <- (f e) 
                    return (e1, rule)
                                    

scoreOptimizations :: ScoreFn -> SymbolTable -> Expr -> [(Expr, String)] -> ThrowsError [(Expr, Int, String)]
scoreOptimizations fn tbl t opts = mapM (scoreOpt t) opts where
                   (Right origFLOPs) = fn t tbl
                   scoreOpt t (t2, rule) = case (fn t2 tbl) of
                                            (Right newFLOPs) -> return (t2, newFLOPs - origFLOPs, rule)
                                            (Left err) -> throwError $ BadOptimization t t2 err
                                    
-- Take a zipper representing a subtree, and a new subtree to replace that subtree.
-- return a full (rooted) tree with the new subtree in the appropriate place.
reconstructTree :: MZipper -> Expr -> Expr
reconstructTree (_, bs) t2 = zipperToTree $ topMost (t2, bs)

reconstructTreeScore:: MZipper -> (Expr, Int, String) -> (Expr, Int, String)
reconstructTreeScore (_, bs) (t2, score, rule) = (zipperToTree $ topMost (t2, bs), score, rule)

-- Utility function used by optimizeAtNode: apply a list of functions
-- to a constant input, silently discarding any element for which f
-- returns Nothing.
mapMaybeFunc :: a -> [(a -> Maybe b)] -> [b]
mapMaybeFunc _ []     = []
mapMaybeFunc x (f:fs) =
  case f x of
    Just y  -> y : mapMaybeFunc x fs
    Nothing -> mapMaybeFunc x fs


recognizeVar :: VarName -> Expr -> SymbolTable -> Expr -> Maybe Expr
recognizeVar var rhs tbl tree = if rhs==tree then Just (Leaf var) else Nothing

