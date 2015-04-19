module Optimization where

import Data.Maybe
import qualified Data.Map as Map
import qualified Data.MultiMap as MultiMap
import qualified Data.Set as Set
import Data.List
import Data.Ord

import Control.Monad.Error

import MTypes
import Analysis

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

optimize :: Expr -> SymbolTable -> ThrowsError (Expr, Int)
optimize expr tbl = do beam <- beamSearch 5 20 4 tbl [(expr, 0)]
                       return $ head beam

----------------------------------------------------------------

-- recursive wrapper function to iterate beam search
beamSearch :: Int -> Int -> Int -> SymbolTable -> [(Expr, Int)] -> ThrowsError [(Expr, Int)]
beamSearch 0 _ _ _ beam = return $ beam
beamSearch iters beamSize nRewrites tbl beam = 
                 do newBeam1 <- beamIter rewriteMoves beamSize nRewrites tbl beam
                    newBeam2 <- beamIter commonSubexpMoves beamSize 1 tbl newBeam1
                    beamSearch (iters-1) beamSize nRewrites tbl newBeam2

type MoveRewriter = SymbolTable -> Expr -> ThrowsError [(Expr, Int)]

-- single iteration of beam search: compute all rewrites and take the best few
beamIter :: MoveRewriter -> Int -> Int -> SymbolTable -> [(Expr, Int)] -> ThrowsError [(Expr, Int)]
beamIter rw beamSize nRewrites tbl oldBeam = do rewrites <- reOptimize nRewrites rw tbl oldBeam
                                                return $ take beamSize (sortBy (comparing snd) rewrites)

-- for cse, same pattern of generating a list of rewrites and sorting them
-- 'reoptimize n' takes a symboltable and a list of candidates, and generates a new list
-- reoptimizeOnce is just reOptimize N=1, minus deduping
-- optimizerTraversal takes a table and a breadCrumb, and generates a list of (expr,scores). 




-- generate all scored rewrites accessible by applying at most n rewrite rules
reOptimize :: Int -> MoveRewriter -> SymbolTable -> [(Expr, Int)] -> ThrowsError [(Expr, Int)]
reOptimize 0 rw tbl candidates = return $ candidates
reOptimize n rw tbl candidates = 
           do iter1 <- reOptimizeOnce rw tbl candidates
              reOptimize (n-1) rw tbl (Set.toList $ Set.fromList (candidates ++ iter1))


-- generate all rewrites of the given scored list of expressions, tracking the 
-- global FLOP delta for each rewrite
reOptimizeOnce :: MoveRewriter -> SymbolTable -> [(Expr, Int)] -> ThrowsError [(Expr, Int)]
reOptimizeOnce _ _ [] = return $ []
reOptimizeOnce rw tbl ((t, score):ts) = do localDeltas <- rw tbl t
                                           newTs <- reOptimizeOnce rw tbl ts
                                           let globalDeltas =  [(r, score+ds) | (r, ds) <- localDeltas] in
                                               return $ globalDeltas ++ newTs

----------------------------------------------------------------

-- clean up tmp variables:
--  - first split the program into lists of tmp and non-tmp statements
--  - for each non-tmp statement, make a list of all the

variablesUsed :: Expr -> [VarName] -> [VarName]
variablesUsed (Leaf a) vs = a:vs
variablesUsed (IdentityLeaf _ ) vs = vs
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
factorSubExpression :: Expr -> SymbolTable -> (Expr, [Breadcrumbs]) -> ThrowsError (Expr, Int)
factorSubExpression e tbl (subexp, locs) = let tmpVar = chooseTmpVarname e in 
                                           do factoredOut <- replaceSubExprs e locs tmpVar
                                              (ntbl, factoredIn) <- insertCommonDef tbl factoredOut tmpVar subexp 
                                              flops <- treeFLOPs subexp ntbl 
                                              let score = (1-(length locs)) * flops + letcost_CONST in
                                                  return $ (factoredIn, score)


-- Given an expression, return all versions reachable by factoring out a single subexpression
commonSubexpMoves :: MoveRewriter
commonSubexpMoves tbl e = let smap = buildSubexpressionMap MultiMap.empty (e, [])
                              subexprs = commonSubExpressions smap in
                          mapM (factorSubExpression e tbl) subexprs
                          


-------------------------------------------------------------------------------------

-- Given a zipper corresponding to a position (node) in a tree, return
-- the list of all new trees constructable by applying a single
-- optimization rule either at the current node, or (recursively) at
-- any descendant node. Note: the transformed trees we return are
-- rooted at the toplevel, i.e. they have been 'unzipped' by
-- reconstructTree.

rewriteMoves :: MoveRewriter
rewriteMoves tbl e = return $ optimizerTraversal tbl (e, [])

optimizerTraversal :: SymbolTable -> MZipper -> [(Expr, Int)]
optimizerTraversal _ (Leaf _, _) = []
optimizerTraversal _ (IdentityLeaf _, _) = []
optimizerTraversal tbl z@( n@(Branch3 _ _ _ _), _) =
        (map (reconstructTreeScore z) (optimizeAtNode tbl n) ) ++
        (trapError [] id (fmap (optimizerTraversal tbl) (goLeft z) )) ++
        (trapError [] id (fmap (optimizerTraversal tbl) (goDown z) )) ++
        (trapError [] id (fmap (optimizerTraversal tbl) (goRight z)))
optimizerTraversal tbl z@( n@(Branch2 _ _ _), _) =
        (map (reconstructTreeScore z) (optimizeAtNode tbl n) ) ++
        (trapError [] id (fmap (optimizerTraversal tbl) (goLeft z) )) ++
        (trapError [] id (fmap (optimizerTraversal tbl) (goRight z)))
optimizerTraversal tbl z@( n@(Let _ _ _ _), _) =
        (map (reconstructTreeScore z) (optimizeAtNode tbl n) ) ++
        (trapError [] id (fmap (optimizerTraversal tbl) (goLeft z) )) ++
        (trapError [] id (fmap (optimizerTraversal boundTbl ) (goRight z)))
        where
        (Right boundTbl) = tblBind n tbl
optimizerTraversal tbl z@( n@(Branch1 _ _), _) =
        (map (reconstructTreeScore z) (optimizeAtNode tbl n) ) ++
        (trapError [] id (fmap (optimizerTraversal tbl) (goDown z)))

-- Given a tree node, return a list of all transformed nodes that can
-- be generated by applying optimization rules at that node, along with
-- the net change in FLOPs
optimizeAtNode :: SymbolTable -> Expr -> [(Expr, Int)]
optimizeAtNode tbl t = let opts = mapMaybeFunc t [f tbl | f <- optimizationRules] in
                       scoreOptimizations tbl t opts

scoreOptimizations :: SymbolTable -> Expr -> [Expr] -> [(Expr, Int)]
scoreOptimizations tbl t opts = map scoreOpt opts where
                   (Right origFLOPs) = treeFLOPs t tbl
                   scoreOpt t2 = let (Right newFLOPs) = treeFLOPs t2 tbl in
                                 (t2, newFLOPs - origFLOPs)

-- Take a zipper representing a subtree, and a new subtree to replace that subtree.
-- return a full (rooted) tree with the new subtree in the appropriate place.
reconstructTree :: MZipper -> Expr -> Expr
reconstructTree (_, bs) t2 = zipperToTree $ topMost (t2, bs)

reconstructTreeScore:: MZipper -> (Expr, Int) -> (Expr, Int)
reconstructTreeScore (_, bs) (t2, score) = (zipperToTree $ topMost (t2, bs), score)

-- Utility function used by optimizeAtNode: apply a list of functions
-- to a constant input, silently discarding any element for which f
-- returns Nothing.
mapMaybeFunc :: a -> [(a -> Maybe b)] -> [b]
mapMaybeFunc _ []     = []
mapMaybeFunc x (f:fs) =
  case f x of
    Just y  -> y : mapMaybeFunc x fs
    Nothing -> mapMaybeFunc x fs

------------------------------------------------------------------
-- List of optimizations
--
-- An optimization is a function SymbolTable -> Expr -> Maybe
-- Expr. The input tree is assumed to be a subexpression. If the
-- optimization can apply to that subexpression, it returns the
-- transformed subexpression. Otherwise it returns Nothing.
--
-- Note that an optimization does not always need to be helpful:
-- optimizations which increase the number of required FLOPs will be
-- selected against, but are perfectly legal (and sometimes necessary
-- as intermediate steps).
--
-- The major current restriction on optimizations is that they should
-- generate at most a finite group of results: thus 'right-multiply by
-- the identity' is not currently allowed as an optimization, since it
-- generates AI, AII, AIII, etc. and will thus yield an infinitely
-- large set of transformed expressions. This could be fixed in the
-- future by imposing a maximum search depth.
--
-- To add a new optimization: make sure to include it in the list of
-- optimizationRules. This list is consulted by optimizeNode to
-- generate all possible transformed versions of a subtree.

-- TODO: Keeping everything in list is kind of ugly. Probably switch to
-- records.
-- TODO: If we switch to records, probably roll out a lens so that the
-- structure keeps (Expr -> Maybe Expr) but gives back (a -> Expr ->
-- Maybe Expr) (i.e. const) for e.g. binopSumRules. Then remove the
-- extra arguments that are cluttering everything.

type Rule  =  SymbolTable -> Expr -> Maybe Expr
type Rules = [Rule]

binopSumRules :: Rules
binopSumRules = [commonFactorLeft
                , commonFactorRight
                , matrixInvLemmaRight
                ]

binopProductRules :: Rules
binopProductRules = [assocMult
                    , invToLinsolve
                    , mergeToTernaryProduct
                    , factorInverse
                    , factorTranspose
                    , mergeInverse
                    , killIdentity
                    , swapTranspose
                    , distributeMult
                    ]

ternProductRules :: Rules
ternProductRules = [splitTernaryProductLeftAssoc
                   , splitTernaryProductRightAssoc
                   ]

inverseRules :: Rules
inverseRules = [distributeInverse
               , swapInverseTranspose
               , cancelDoubleInverse
               , matrixInvLemmaLeft
               ]

transposeRules :: Rules
transposeRules = [distributeTranspose
                 , swapTransposeInverse
                 , cancelDoubleTranspose
                 ]

letExpRules :: Rules
letExpRules = [groundSubExpr]           

optimizationRules :: Rules
optimizationRules = inverseRules ++ transposeRules ++ binopSumRules ++
    binopProductRules ++ ternProductRules ++ letExpRules

groundSubExpr :: Rule
groundSubExpr _ (Let lhs rhs True body) = Just (groundSubExprHelper body lhs rhs)
groundSubExpr _ _ = Nothing

groundSubExprHelper :: Expr -> VarName -> Expr -> Expr
groundSubExprHelper (Leaf a) v subexpr = if (a == v) then subexpr else (Leaf a)
groundSubExprHelper (Branch1 op a) v subexpr = Branch1 op (groundSubExprHelper a v subexpr)
groundSubExprHelper (Branch2 op a b) v subexpr = Branch2 op (groundSubExprHelper a v subexpr) (groundSubExprHelper b v subexpr)
groundSubExprHelper (Branch3 op a b c) v subexpr = Branch3 op (groundSubExprHelper a v subexpr) (groundSubExprHelper b v subexpr) (groundSubExprHelper c v subexpr)
groundSubExprHelper (Let lhs rhs tmp body) v subexpr = Let lhs (groundSubExprHelper rhs v subexpr) tmp (groundSubExprHelper body v subexpr)
groundSubExprHelper e v subexpr = e

-- (AB)C -> A(BC)
-- A(BC) -> (AB)C
assocMult :: Rule
assocMult _ (Branch2 MProduct (Branch2 MProduct l c) r) = Just (Branch2 MProduct l (Branch2 MProduct c r))
assocMult _ (Branch2 MProduct l (Branch2 MProduct c r)) = Just (Branch2 MProduct (Branch2 MProduct l c) r)
assocMult _ _ = Nothing

-- (AC + BC) -> (A+B)C
commonFactorRight :: Rule
commonFactorRight _ (Branch2 MSum (Branch2 MProduct l1 l2) (Branch2 MProduct r1 r2)) =
  if (l2 == r2)
     then Just (Branch2 MProduct (Branch2 MSum l1 r1) l2)
     else Nothing
commonFactorRight _ _ = Nothing

-- (AB + AC) -> A(B+C)
commonFactorLeft :: Rule
commonFactorLeft _ (Branch2 MSum (Branch2 MProduct l1 l2) (Branch2 MProduct r1 r2)) =
  if (l1 == r1)
     then Just (Branch2 MProduct l1 (Branch2 MSum l2 r2))
     else Nothing
commonFactorLeft _ _ = Nothing

-- A(B+C) -> AB+AC
-- (A+B)C -> AC+BC
distributeMult :: Rule
distributeMult _ (Branch2 MProduct l (Branch2 MSum c r)) = Just (Branch2 MSum (Branch2 MProduct l c) (Branch2 MProduct l r) )
distributeMult _ (Branch2 MProduct (Branch2 MSum l c) r) = Just (Branch2 MSum (Branch2 MProduct l r) (Branch2 MProduct c r) )
distributeMult _ _ = Nothing

-- A^-1 B -> A\B
invToLinsolve :: Rule
invToLinsolve tbl (Branch2 MProduct (Branch1 MInverse l) r) =
        let Right (Matrix _ _ props) = treeMatrix l tbl in
            if PosDef `elem` props
                then Just (Branch2 MCholSolve l r)
                else Just (Branch2 MLinSolve l r)
invToLinsolve _ _ = Nothing

-- A^-1 A -> I
-- AA^-1 -> I
mergeInverse :: Rule
mergeInverse tbl (Branch2 MProduct (Branch1 MInverse l) r) =
             let Right (Matrix n _ _) = treeMatrix l tbl in
                 if (l == r)
                    then Just (IdentityLeaf n)
                    else Nothing
mergeInverse tbl (Branch2 MProduct l (Branch1 MInverse r)) =
             let Right (Matrix n _ _) = treeMatrix l tbl in
                 if (l == r)
                    then Just (IdentityLeaf n)
                    else Nothing
mergeInverse _ _ = Nothing

-- AI -> A
-- IA -> A
killIdentity :: Rule
killIdentity _ (Branch2 MProduct (IdentityLeaf _) r) = Just r
killIdentity _ (Branch2 MProduct l (IdentityLeaf _)) = Just l
killIdentity _ _ = Nothing

-- (AB)C -> ABC
-- A(BC) -> ABC
mergeToTernaryProduct :: Rule
mergeToTernaryProduct _ (Branch2 MProduct (Branch2 MProduct l c) r) =
        Just (Branch3 MTernaryProduct l c r)
mergeToTernaryProduct _ (Branch2 MProduct l (Branch2 MProduct c r)) =
        Just (Branch3 MTernaryProduct l c r)
mergeToTernaryProduct _ _ = Nothing

-- ABC -> (AB)C
splitTernaryProductLeftAssoc :: Rule
splitTernaryProductLeftAssoc _ (Branch3 MTernaryProduct l c r) =
        Just (Branch2 MProduct (Branch2 MProduct l c) r)
splitTernaryProductLeftAssoc _ _ = Nothing

-- ABC -> A(BC)
splitTernaryProductRightAssoc :: Rule
splitTernaryProductRightAssoc _ (Branch3 MTernaryProduct l c r) =
        Just (Branch2 MProduct l (Branch2 MProduct c r))
splitTernaryProductRightAssoc _ _ = Nothing

-- (AB)^-1 -> B^-1 A^-1 as long as A and B are both square
distributeInverse :: Rule
distributeInverse tbl (Branch1 MInverse (Branch2 MProduct l r)) =
                  let Right (Matrix lr lc _) = treeMatrix l tbl
                      Right (Matrix rr rc _) = treeMatrix r tbl in
                  if (lr == lc) && (rr == rc)
                  then Just (Branch2 MProduct (Branch1 MInverse r)
                                              (Branch1 MInverse l))
                  else Nothing
distributeInverse _ _ = Nothing

-- B^-1 A^-1 -> (AB)^-1
factorInverse :: Rule
factorInverse _ (Branch2 MProduct (Branch1 MInverse r) (Branch1 MInverse l)) =
        Just (Branch1 MInverse (Branch2 MProduct l r))
factorInverse _ _ = Nothing

-- A^-1^-1 -> A
cancelDoubleInverse ::Rule
cancelDoubleInverse _ (Branch1 MInverse (Branch1 MInverse t)) = Just t
cancelDoubleInverse _ _ = Nothing

-- (AB)' -> (B'A')
distributeTranspose :: Rule
distributeTranspose _ (Branch1 MTranspose (Branch2 MProduct l r)) =
        Just (Branch2 MProduct (Branch1 MTranspose r) (Branch1 MTranspose l))
distributeTranspose _ _ = Nothing

-- (B'A') -> (AB)'
factorTranspose :: Rule
factorTranspose _ (Branch2 MProduct (Branch1 MTranspose r) (Branch1 MTranspose l)) =
        Just (Branch1 MTranspose (Branch2 MProduct l r))
factorTranspose _ _ = Nothing

-- (A'B) -> (B'A)'
-- (this rule is its own inverse when combined with cancelDoubleTranspose)
swapTranspose :: Rule
swapTranspose _ (Branch2 MProduct (Branch1 MTranspose l) r) = Just (Branch1 MTranspose (Branch2 MProduct (Branch1 MTranspose r) l))
swapTranspose _ _ = Nothing

-- A'' = A
cancelDoubleTranspose :: Rule
cancelDoubleTranspose _ (Branch1 MTranspose (Branch1 MTranspose t)) = Just t
cancelDoubleTranspose _ _ = Nothing

-- A'^-1 -> A^-1'
swapInverseTranspose :: Rule
swapInverseTranspose _ (Branch1 MInverse (Branch1 MTranspose t)) =
        Just (Branch1 MTranspose (Branch1 MInverse t))
swapInverseTranspose _ _ = Nothing

-- A^-1' -> A'^-1
swapTransposeInverse :: Rule
swapTransposeInverse _ (Branch1 MTranspose (Branch1 MInverse t)) =
        Just (Branch1 MInverse (Branch1 MTranspose t))
swapTransposeInverse _ _ = Nothing

-- (A+UCV)^-1 -> A^-1 - A^-1 U (C^-1 + V A^-1 U)^-1 V A^-1
matrixInvLemmaLeft :: Rule
matrixInvLemmaLeft _ (Branch1 MInverse (Branch2 MSum a (Branch3 MTernaryProduct u c v))) =
  Just (Branch2 MSum (Branch1 MInverse a) (Branch1 MNegate (Branch3 MTernaryProduct (Branch2 MProduct (Branch1 MInverse a) u ) (Branch1 MInverse ( Branch2 MSum (Branch1 MInverse c) (Branch3 MTernaryProduct v (Branch1 MInverse a) u) ) ) (Branch2 MProduct v (Branch1 MInverse a)) )))
matrixInvLemmaLeft _ _ = Nothing

--  A^-1 - A^-1 U (C^-1 + V A^-1 U)^-1 V A^-1 -> (A+UCV)^-1
matrixInvLemmaRight :: Rule
matrixInvLemmaRight tbl (Branch2 MSum (Branch1 MInverse a) (Branch1 MNegate (Branch3 MTernaryProduct (Branch2 MProduct (Branch1 MInverse b) c ) (Branch1 MInverse ( Branch2 MSum (Branch1 MInverse d) (Branch3 MTernaryProduct e (Branch1 MInverse f) g) )) (Branch2 MProduct h (Branch1 MInverse i)) ))) =
                    if (a == b && a == f && a == i)&& (c == g)&& (e == h) then
                    -- MIL A: (A, B, F, I)
                    -- MIL U: C, G
                    -- MIL C: D
                    -- MIL V: E, H
                    Just (Branch1 MInverse (Branch2 MSum a (Branch3 MTernaryProduct c d e)))    
                    else Nothing
matrixInvLemmaRight tbl z@(Branch2 MSum ainv (Branch1 MNegate (Branch3 MTernaryProduct (Branch2 MProduct ainv2 u ) (Branch1 MInverse ( Branch2 MSum cinv (Branch3 MTernaryProduct v ainv3 u2) ) ) (Branch2 MProduct v2 ainv4) ) ) ) =
   if (ainv == ainv2 && ainv == ainv3 && ainv == ainv4)&& (u == u2)&& (v == v2)
   then Just (Branch1 MInverse (Branch2 MSum (Branch1 MInverse ainv) (Branch3 MTernaryProduct u (Branch1 MInverse cinv) v)))
  else Nothing
matrixInvLemmaRight _ _ = Nothing