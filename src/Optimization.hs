module Optimization where

import Data.Maybe
import qualified Data.Map as Map
import qualified Data.MultiMap as MultiMap
import qualified Data.Set as Set

import Data.List

import MTypes
import Analysis

import Debug.Trace

--------------------------------------------------------------------------------------------------------
-- Zipper definitions for the Expr structure (see http://learnyouahaskell.com/zippers)

data Crumb = SingleCrumb UnOp
           | LeftCrumb BinOp Expr
           | RightCrumb BinOp Expr
           | TernCrumb TernOp [Expr] [Expr]
           deriving (Show, Eq)
type Breadcrumbs = [Crumb]

type MZipper = (Expr, Breadcrumbs)

goLeft :: MZipper -> Maybe MZipper
goLeft (Branch2 op l r, bs) = Just (l, LeftCrumb op r:bs)
goLeft (Branch3 op l c r, bs) = Just (l, TernCrumb op [] [c,r] : bs)
goLeft _ = Nothing

goRight :: MZipper -> Maybe MZipper
goRight (Branch2 op l r, bs) = Just (r, RightCrumb op l:bs)
goRight (Branch3 op l c r, bs) = Just (r, TernCrumb op [l,c] [] : bs)
goRight _ = Nothing

goDown :: MZipper -> Maybe MZipper
goDown (Branch1 op t, bs) = Just (t, SingleCrumb op : bs)
goDown (Branch3 op l c r, bs) = Just (c, TernCrumb op [l] [r] : bs)
goDown _ = Nothing

goUp :: MZipper -> Maybe MZipper
goUp (t, SingleCrumb op : bs) = Just (Branch1 op t, bs)
goUp (t, LeftCrumb op r : bs) = Just (Branch2 op t r, bs)
goUp (t, RightCrumb op l : bs) = Just (Branch2 op l t, bs)
goUp (t, TernCrumb op [] [c, r] : bs) = Just (Branch3 op t c r , bs)
goUp (t, TernCrumb op [l] [r] : bs) = Just (Branch3 op l t r , bs)
goUp (t, TernCrumb op [l,c] [] : bs) = Just (Branch3 op l c t , bs)

goUp _ = Nothing

topMost :: MZipper -> MZipper
topMost (t,[]) = (t,[])
topMost z = topMost $ maybe z id (goUp z)

modify :: (Expr -> Maybe Expr) -> MZipper -> MZipper
modify f (t, bs) = case (f t) of
  Just a -> (a, bs)
  Nothing -> (t, bs)

zipperToTree :: MZipper -> Expr
zipperToTree (n, _) = n

-----------------------------------------------------------------
-- Main optimizer logic

-- optimizePrgm :: Program -> SymbolTable -> ThrowsError (Int, Program)
-- optimizePrgm prgm tbl = do (_, firstpass) <- optimizePrgmPass prgm tbl
--                           newTbl <- checkTypes firstpass tbl
--                           optimizePrgmPass firstpass newTbl

optimizePrgm :: Program -> SymbolTable -> ThrowsError Program
optimizePrgm p t = do (p1, t1) <- return $ commonSubExpressionPass p t
                      (_, p1opt) <- optimizePrgmLocal p1 t1
                      p1clean <- return $ cleanTmp p1opt
                      (p2, t2) <- return $ commonSubExpressionPass p1clean t1
                      (_, p2opt) <- optimizePrgmLocal p2 t2
                      return $ cleanTmp p2opt

commonSubExpressionPass :: Program -> SymbolTable -> (Program, SymbolTable)
commonSubExpressionPass prgm tbl = let smap = buildGlobalSubExpressionMap prgm
                                       subexprs = commonSubExpressions smap in
                                   foldl factorSubExpression (prgm, tbl) subexprs

optimizePrgmLocal :: Program -> SymbolTable -> ThrowsError (Int, Program)
optimizePrgmLocal (Seq x) tbl = do pairs <- mapM optimizeStmt x
                                   let (flops, stmts) = unzip pairs in
                                       return (sum flops, Seq stmts)
                                where
                                optimizeStmt (Assign v e tmp) = do (flops, eopt) <- optimizeExpr e tbl
                                                                   return $ (flops, Assign v eopt tmp)

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

allVariablesUsed :: Program  -> [VarName]
allVariablesUsed (Seq ((Assign _ e _):xs)) = variablesUsed e (allVariablesUsed (Seq xs))
allVariablesUsed _ = []

cleanTmp :: Program -> Program
cleanTmp prgm = let vs = allVariablesUsed prgm
                    (Seq stmts) = prgm in
                    Seq $ removeUnused stmts vs
                where
                removeUnused (x@(Assign v _ True):xs) used = if v `elem` used
                                                                   then x : (removeUnused xs used)
                                                                   else removeUnused xs used
                removeUnused (x:xs) used = x : (removeUnused xs used)
                removeUnused stmts _  = stmts


-- replaceVar :: Expr -> VarName -> Expr -> Expr
--replaceVar v sube (Leaf a)  = if a == v
--                             then sube
--                             else e
--replaceVar v sube (Branch1 op a) = Branch1 op (replaceVar a v sube)
--replaceVar v sube (Branch2 op a b) = Branch2 op (replaceVar a v sube) (replaceVar b v sube)
--replaceVar v sube (Branch3 op a b c) = Branch3 op (replaceVar a v sube) (replaceVar b v sube) (replaceVar c v sube)

---------------------------------------------------------------

-- common subexpression elimination code
type SubExprLoc = (VarName, Breadcrumbs)
type SubExprMap = MultiMap.MultiMap Expr SubExprLoc

buildGlobalSubExpressionMap :: Program -> SubExprMap
buildGlobalSubExpressionMap (Seq xs) = mergeMultiMaps (map buildStmtMap xs) where
            buildStmtMap (Assign v e _) = buildSubexpressionMap MultiMap.empty  v (e, [])
            -- merge values from a list of MultiMaps into a single Map
            mergeMultiMaps :: (Ord a) => [MultiMap.MultiMap a b] -> MultiMap.MultiMap a b
            mergeMultiMaps multimaps = let maps = map MultiMap.toMap multimaps
                                           oneMap = Map.unionsWith (++) maps in
                                       MultiMap.fromMap oneMap

-- build a subexpression map for a given expression
buildSubexpressionMap :: SubExprMap -> VarName -> MZipper -> SubExprMap
buildSubexpressionMap smap stmt z@( n@(Branch1 _ _), bs) =
                      let childMap = maybe smap (buildSubexpressionMap smap stmt) (goDown z) in
                      MultiMap.insert n (stmt, bs) childMap
buildSubexpressionMap smap stmt z@( n@(Branch2 _ _ _), bs) =
                      let leftMap = maybe smap (buildSubexpressionMap smap stmt) (goLeft z)
                          rightMap = maybe leftMap (buildSubexpressionMap leftMap stmt) (goRight z) in
                      MultiMap.insert n (stmt, bs) rightMap
buildSubexpressionMap smap stmt z@( n@(Branch3 _ _ _ _), bs) =
                      let leftMap = maybe smap (buildSubexpressionMap smap stmt) (goLeft z)
                          centerMap = maybe leftMap (buildSubexpressionMap leftMap stmt) (goDown z)
                          rightMap = maybe centerMap (buildSubexpressionMap centerMap stmt) (goRight z) in
                      MultiMap.insert n (stmt, bs) rightMap
buildSubexpressionMap smap _ (Leaf _ , _) = smap
buildSubexpressionMap smap stmt (n@(IdentityLeaf _), bs) = MultiMap.insert n (stmt, bs) smap


-- given a character specifying a given statement, remove all of those expressions from the map
removeLineFromMap :: SubExprMap -> VarName -> SubExprMap
removeLineFromMap smap v = let m = MultiMap.toMap smap
                               filtered_m = Map.map (filter (\(vv, _) -> (vv == v) )) m in
                           MultiMap.fromMap filtered_m

updateMapWithLine :: SubExprMap -> Stmt -> SubExprMap
updateMapWithLine smap (Assign v e _) = buildSubexpressionMap smap v (e, [])

-- generate a list of all common subexpressions: expressions that occur at least twice
commonSubExpressions :: SubExprMap -> [(Expr, [SubExprLoc])]
commonSubExpressions smap = let l = Map.toList $ MultiMap.toMap smap
                                l_notrans = filter (\(e, _) -> not $ isTranspose e) l in
                            filter (\(_, locs) -> (length locs > 1)) l_notrans
                            where isTranspose (Branch1 MTranspose (Leaf _)) = True
                                  isTranspose _ = False

-- given a subexpression, return the program transformed to factor out
-- that subexpression into its own statement.
factorSubExpression :: (Program, SymbolTable) -> (Expr, [SubExprLoc]) -> (Program, SymbolTable)
factorSubExpression ((Seq stmts), tbl) (e, locs) =
    let currentVarMatch = find (\(v, bs) -> null bs) locs in
    subCurrentVar ((Seq stmts), tbl) (e, locs) currentVarMatch

-- deal with two cases: either we already have a variable assigned to
-- the subexpression in question, or we need to create a new tmp variable.
-- this is a bit of a hack and should probably be incorporated under factorSubExpression
subCurrentVar ((Seq stmts), tbl) (e, locs) (Just (v, bs)) =
              let (Just idx) = stmtPos stmts v
                  (p, ps) = splitAt (idx + 1) stmts in
              (Seq $ p ++ subAll ps v locs, tbl)
subCurrentVar ((Seq stmts), tbl) (e, locs) Nothing =
              let newVar = getNewVar tbl
                  (Right newMatrix) = (treeMatrix e tbl)
                  newTbl = Map.insert newVar newMatrix tbl
                  newStmts = subAll stmts newVar locs
                  newIdx = minimum $ catMaybes $ map (stmtPos newStmts) (fst $ unzip locs)
                  (p,ps) = splitAt newIdx newStmts in
              (Seq $ p ++ (Assign newVar e True):ps, newTbl)
              where
              getNewVar oldTbl = maybe "tmp11" id (find ((flip Map.notMember) oldTbl) ["tmp1", "tmp2", "tmp3", "tmp4", "tmp5", "tmp6", "tmp7", "tmp8", "tmp9", "tmp10"])

stmtPos stmtList v = findIndex (\(Assign vv _ _) -> (vv == v)) stmtList



-- given a list of locations where a particular subexpression occurs,
-- return the program in which all of those locations are replaced by
-- the given variable
subAll :: [Stmt] -> VarName -> [SubExprLoc] -> [Stmt]
subAll stmts newVar (x:xs) = subAll (replaceSubExpInPrgm stmts newVar x) newVar xs
subAll stmts _ [] = stmts

replaceSubExpInPrgm :: [Stmt] -> VarName -> SubExprLoc -> [Stmt]
replaceSubExpInPrgm stmts newVar (vv, bs) = map (replaceSubExp newVar (vv, bs)) stmts

replaceSubExp :: VarName -> SubExprLoc -> Stmt -> Stmt
replaceSubExp newVar (vv, bs) (Assign v e tmp)  =
              if (v /= vv)
              then Assign v e tmp
              else let newZipper = recreateZipper (reverse bs) (Just (e, [])) in
              case newZipper of
              (Just z) -> Assign vv (reconstructTree z (Leaf newVar)) tmp
              Nothing -> Assign vv e tmp

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
recreateZipper :: Breadcrumbs -> Maybe MZipper -> Maybe MZipper
recreateZipper (LeftCrumb _ _:bs) (Just z) = recreateZipper bs (goLeft z)
recreateZipper ((RightCrumb _ _):bs) (Just z) = recreateZipper bs (goRight z)
recreateZipper ((SingleCrumb _):bs) (Just z) = recreateZipper bs (goDown z)
recreateZipper ((TernCrumb _ _ _):bs) (Just z) = recreateZipper bs (goDown z)
recreateZipper [] z = z
recreateZipper _ Nothing = Nothing

----------------------------------------------------------------
-- toplevel expression optimization function: first, call optimizeHelper to
--  get a list of all transformed versions of the current tree
--  (applying all rules at every node until no new trees are
--  produced). Second, calculate FLOPs for each of the transformed
--  trees (note technically the FLOPs calculation can fail, so we get
--  sketchyFLOPsList which is a list of ThrowsError Int, hence the
--  final fmap which deals with this).  Finally, sort the zipped
--  (flops, trees) list to get the tree with the smallest FLOP count,
--  and return that.
optimizeExpr :: Expr -> SymbolTable -> ThrowsError (Int, Expr)
optimizeExpr tree tbl = let (_, allTreesSet) = optimizeHelper tbl [tree] (Set.singleton tree)
                            allTreesList = Set.toList allTreesSet
                            sketchyFLOPsList = mapM (flip treeFLOPs tbl) allTreesList in
                        fmap (\ flopsList -> head $ sort $ zip flopsList allTreesList) sketchyFLOPsList


-- inputs: a list of still-to-be-transformed expressions, and a set of
-- all expressions that have already been generated by the
-- optimization rules

-- outputs: a new list of candidate expressions, constructed by
-- removing the first element of the previous list, and appending to
-- the end all legal transformations of that element that are not
-- already in the tabu set. also returns a augmented tabu set
-- containing all of the newly generated expressions (the same ones
-- that were added to the list)
type TabuSet = Set.Set Expr
optimizeHelper :: SymbolTable -> [Expr] -> TabuSet -> ([Expr], TabuSet)
optimizeHelper _ [] exprSet = ([], exprSet)
optimizeHelper tbl (t:ts) exprSet = let generatedExprs = Set.fromList $ optimizerTraversal tbl (t, [])
                                        novelExprs = Set.difference generatedExprs exprSet in
                                    -- trace ("oH " ++ (show generatedExprs) ++ (show novelExprs) ++ (show exprSet)) $
                                    optimizeHelper tbl ( ts ++ (Set.toList novelExprs) ) (Set.union exprSet novelExprs)

-- Given a zipper corresponding to a position (node) in a tree, return
-- the list of all new trees constructable by applying a single
-- optimization rule either at the current node, or (recursively) at
-- any descendant node. Note: the transformed trees we return are
-- rooted at the toplevel, i.e. they have been 'unzipped' by
-- reconstructTree.
optimizerTraversal :: SymbolTable -> MZipper -> [Expr]
optimizerTraversal _ (Leaf _, _) = []
optimizerTraversal _ (IdentityLeaf _, _) = []
optimizerTraversal tbl z@( n@(Branch3 _ _ _ _), _) =
        (map (reconstructTree z) (optimizeAtNode tbl n) ) ++
        (maybe [] id (fmap (optimizerTraversal tbl) (goLeft z) )) ++
        (maybe [] id (fmap (optimizerTraversal tbl) (goDown z) )) ++
        (maybe [] id (fmap (optimizerTraversal tbl) (goRight z)))
optimizerTraversal tbl z@( n@(Branch2 _ _ _), _) =
        (map (reconstructTree z) (optimizeAtNode tbl n) ) ++
        (maybe [] id (fmap (optimizerTraversal tbl) (goLeft z) )) ++
        (maybe [] id (fmap (optimizerTraversal tbl) (goRight z)))
optimizerTraversal tbl z@( n@(Branch1 _ _), _) =
        (map (reconstructTree z) (optimizeAtNode tbl n) ) ++
        (maybe [] id (fmap (optimizerTraversal tbl) (goDown z)))

-- Given a tree node, return a list of all transformed nodes that can
-- be generated by applying optimization rules at that node.
optimizeAtNode :: SymbolTable -> Expr -> [Expr]
optimizeAtNode tbl t = mapMaybeFunc t [f tbl | f <- optimizationRules]

-- Take a zipper representing a subtree, and a new subtree to replace that subtree.
-- return a full (rooted) tree with the new subtree in the appropriate place.
reconstructTree :: MZipper -> Expr -> Expr
reconstructTree (_, bs) t2 = zipperToTree $ topMost (t2, bs)

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
--                , matrixInvLemmaRight
                ]

binopProductRules :: Rules
binopProductRules = [assocMult
                    , invToLinsolve
                    , mergeToTernaryProduct
                    , factorInverse
                    , factorTranspose
                    , mergeInverse
                    , killIdentity
                    ]

ternProductRules :: Rules
ternProductRules = [splitTernaryProductLeftAssoc
                   , splitTernaryProductRightAssoc
                   ]

inverseRules :: Rules
inverseRules = [distributeInverse
               , swapInverseTranspose
               , cancelDoubleInverse,
                 matrixInvLemmaLeft
               ]

transposeRules :: Rules
transposeRules = [distributeTranspose
                 , swapTransposeInverse
                 ]

optimizationRules :: Rules
optimizationRules = inverseRules ++ transposeRules ++ binopSumRules ++
    binopProductRules ++ ternProductRules

assocMult :: Rule
assocMult _ (Branch2 MProduct (Branch2 MProduct l c) r) = Just (Branch2 MProduct l (Branch2 MProduct c r))
assocMult _ (Branch2 MProduct l (Branch2 MProduct c r)) = Just (Branch2 MProduct (Branch2 MProduct l c) r)
assocMult _ _ = Nothing

commonFactorRight :: Rule
commonFactorRight _ (Branch2 MSum (Branch2 MProduct l1 l2) (Branch2 MProduct r1 r2)) =
  if (l2 == r2)
     then Just (Branch2 MProduct (Branch2 MSum l1 r1) l2)
     else Nothing
commonFactorRight _ _ = Nothing

commonFactorLeft :: Rule
commonFactorLeft _ (Branch2 MSum (Branch2 MProduct l1 l2) (Branch2 MProduct r1 r2)) =
  if (l1 == r1)
     then Just (Branch2 MProduct l1 (Branch2 MSum l2 r2))
     else Nothing
commonFactorLeft _ _ = Nothing

invToLinsolve :: Rule
invToLinsolve tbl (Branch2 MProduct (Branch1 MInverse l) r) =
        let Right (Matrix _ _ props) = treeMatrix l tbl in
            if PosDef `elem` props
                then Just (Branch2 MCholSolve l r)
                else Just (Branch2 MLinSolve l r)
invToLinsolve _ _ = Nothing

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


killIdentity :: Rule
killIdentity _ (Branch2 MProduct (IdentityLeaf _) r) = Just r
killIdentity _ (Branch2 MProduct l (IdentityLeaf _)) = Just l
killIdentity _ _ = Nothing


mergeToTernaryProduct :: Rule
mergeToTernaryProduct _ (Branch2 MProduct (Branch2 MProduct l c) r) =
        Just (Branch3 MTernaryProduct l c r)
mergeToTernaryProduct _ (Branch2 MProduct l (Branch2 MProduct c r)) =
        Just (Branch3 MTernaryProduct l c r)
mergeToTernaryProduct _ _ = Nothing

splitTernaryProductLeftAssoc :: Rule
splitTernaryProductLeftAssoc _ (Branch3 MTernaryProduct l c r) =
        Just (Branch2 MProduct (Branch2 MProduct l c) r)
splitTernaryProductLeftAssoc _ _ = Nothing

splitTernaryProductRightAssoc :: Rule
splitTernaryProductRightAssoc _ (Branch3 MTernaryProduct l c r) =
        Just (Branch2 MProduct l (Branch2 MProduct c r))
splitTernaryProductRightAssoc _ _ = Nothing

-- we can do (AB)^-1 = B^-1 A^-1 as long as A and B are both square
distributeInverse :: Rule
distributeInverse tbl (Branch1 MInverse (Branch2 MProduct l r)) =
                  let Right (Matrix lr lc _) = treeMatrix l tbl
                      Right (Matrix rr rc _) = treeMatrix r tbl in
                  if (lr == lc) && (rr == rc)
                  then Just (Branch2 MProduct (Branch1 MInverse r)
                                              (Branch1 MInverse l))
                  else Nothing
distributeInverse _ _ = Nothing

factorInverse :: Rule
factorInverse _ (Branch2 MProduct (Branch1 MInverse r) (Branch1 MInverse l)) =
        Just (Branch1 MInverse (Branch2 MProduct l r))
factorInverse _ _ = Nothing

cancelDoubleInverse ::Rule
cancelDoubleInverse _ (Branch1 MInverse (Branch1 MInverse t)) = Just t
cancelDoubleInverse _ _ = Nothing

distributeTranspose :: Rule
distributeTranspose _ (Branch1 MTranspose (Branch2 MProduct l r)) =
        Just (Branch2 MProduct (Branch1 MTranspose r) (Branch1 MTranspose l))
distributeTranspose _ _ = Nothing

factorTranspose :: Rule
factorTranspose _ (Branch2 MProduct (Branch1 MTranspose r) (Branch1 MTranspose l)) =
        Just (Branch1 MTranspose (Branch2 MProduct l r))
factorTranspose _ _ = Nothing

swapInverseTranspose :: Rule
swapInverseTranspose _ (Branch1 MInverse (Branch1 MTranspose t)) =
        Just (Branch1 MTranspose (Branch1 MInverse t))
swapInverseTranspose _ _ = Nothing

swapTransposeInverse :: Rule
swapTransposeInverse _ (Branch1 MTranspose (Branch1 MInverse t)) =
        Just (Branch1 MInverse (Branch1 MTranspose t))
swapTransposeInverse _ _ = Nothing

matrixInvLemmaLeft :: Rule
matrixInvLemmaLeft _ (Branch1 MInverse (Branch2 MSum a (Branch3 MTernaryProduct u c v))) =
  Just (Branch2 MSum (Branch1 MInverse a) (Branch1 MNegate (Branch3 MTernaryProduct (Branch2 MProduct (Branch1 MInverse a) u ) ( Branch2 MSum (Branch1 MInverse c) (Branch3 MTernaryProduct v (Branch1 MInverse a) u) ) (Branch2 MProduct v (Branch1 MInverse a)) )))
matrixInvLemmaLeft _ _ = Nothing

-- matrixInvLemmaRight :: Rule
--matrixInvLemmaRight tbl (Branch2 MSum (Branch1 MInverse A) (Branch1 MNegate (Branch3 MTernaryProduct (Branch2 MProduct (Branch1 MInverse B) C ) ( Branch2 MSum (Branch1 MInverse D) (Branch3 MTernaryProduct E (Branch1 MInverse F) G) ) (Branch2 MProduct H (Branch1 MInverse I)) ))) =if (A == B && A == F && A == I)&& (C == G)&& (D == H)
-- matrixInvLemmaRight _ (Branch2 MSum ainv (Branch1 MNegate (Branch3 MTernaryProduct (Branch2 MProduct ainv2 u ) ( Branch2 MSum cinv (Branch3 MTernaryProduct v ainv3 u2) ) (Branch2 MProduct v2 ainv4) ))) =
--   if (ainv == ainv2 && ainv == ainv3 && ainv == ainv4)&& (u == u2)&& (v == v2)
--  then Just (Branch1 MInverse (Branch2 MSum (Branch1 MInverse ainv) (Branch3 MTernaryProduct u (Branch1 MInverse cinv) v)))
--  else Nothing
-- matrixInvLemmaRight _ _ = Nothing