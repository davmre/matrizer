module Matrizer.RewriteRules (
 Rule,
 Rules,
 optimizationRules,
 optimizationRulesFull,
) where 

import Data.Maybe

import Matrizer.MTypes
import Matrizer.Analysis

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

optimizationRules :: Rules
optimizationRules = inverseRules ++ transposeRules ++ binopSumRules ++
    binopProductRules ++ ternProductRules ++ letExpRules ++ traceRules ++ detRules ++ diagRules ++ entrySumRules ++ hadamardProductRules ++ elementWiseRules

-- moves that are valid and sometimes necessary, but generate many
-- matches and can slow down inference. 
expensiveMoves :: Rules
expensiveMoves = [introduceTranspose]

optimizationRulesFull = optimizationRules ++ expensiveMoves

binopSumRules :: Rules
binopSumRules = [commonFactorLeft
                , commonFactorRight
                , matrixInvLemmaRight
                , assocSum
                , commuteSum
                , dropZeros 
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
                    , literalScalars
                    , commuteScalarProduct
                    , assocScalarProduct
                    , collectTerms
                    , multDiag
                    , diffToScalar
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
               , invariantIdentity
               , invToCholInv
               , cholSolvetoTri
               ]

transposeRules :: Rules
transposeRules = [distributeTranspose
                 , swapTransposeInverse
                 , cancelTranspose
--                 , symTranspose
                 ]

traceRules :: Rules
traceRules = [dissolveTrace
              , commuteTrace
              , transposeTrace
              , linearTrace
              , identityOps
              , traceProduct
              , traceDiag]

detRules :: Rules
detRules = [factorDet
            , detProps
            , logdet]

diagRules :: Rules
diagRules = [cancelDiag
             , invDiag]

entrySumRules :: Rules
entrySumRules = [entrySumLinear]

elementWiseRules :: Rules
elementWiseRules = [elementUnOps]

hadamardProductRules :: Rules
hadamardProductRules = [hadamardProductAssoc, hadamardProductDist, hadamardProductCommute]

letExpRules :: Rules
letExpRules = [groundSubExpr]           


groundSubExpr :: Rule
groundSubExpr _ (Let lhs rhs True body) = Just (groundSubExprHelper body lhs rhs)
groundSubExpr _ _ = Nothing

groundSubExprHelper :: Expr -> VarName -> Expr -> Expr
groundSubExprHelper (Leaf a) v subexpr = if (a == v) then subexpr else (Leaf a)
groundSubExprHelper (Branch1 op a) v subexpr = Branch1 op (groundSubExprHelper a v subexpr)
groundSubExprHelper (Branch2 op a b) v subexpr = Branch2 op (groundSubExprHelper a v subexpr) (groundSubExprHelper b v subexpr)
groundSubExprHelper (Branch3 op a b c) v subexpr = Branch3 op (groundSubExprHelper a v subexpr) (groundSubExprHelper b v subexpr) (groundSubExprHelper c v subexpr)
groundSubExprHelper (Let lhs rhs tmp body) v subexpr = 
                    if rhs == (Leaf v) -- special case of an existing variable that is redundant with the one we're substituting
                    then Let lhs (groundSubExprHelper rhs v subexpr) tmp (groundSubExprHelper body v (Leaf lhs))
                    else Let lhs (groundSubExprHelper rhs v subexpr) tmp (groundSubExprHelper body v subexpr)
groundSubExprHelper e v subexpr = e


-- (A+B)+C <-> A+(B+C)
assocSum :: Rule
assocSum _ (Branch2 MSum (Branch2 MSum l c) r) = Just (Branch2 MSum l (Branch2 MSum c r))
assocSum _ (Branch2 MSum l (Branch2 MSum c r)) = Just (Branch2 MSum (Branch2 MSum l c) r)
assocSum _ _ = Nothing

-- A+B <-> B+A
commuteSum :: Rule
commuteSum _ (Branch2 MSum a b) = Just (Branch2 MSum b a)
commuteSum _ _ = Nothing

-- (AB)C -> A(BC)
-- A(BC) -> (AB)C
assocMult :: Rule
assocMult _ (Branch2 MProduct (Branch2 MProduct l c) r) = Just (Branch2 MProduct l (Branch2 MProduct c r))
assocMult _ (Branch2 MProduct l (Branch2 MProduct c r)) = Just (Branch2 MProduct (Branch2 MProduct l c) r)
assocMult _ (Branch2 MScalarProduct (Branch2 MScalarProduct l c) r) = Just (Branch2 MScalarProduct l (Branch2 MScalarProduct c r))
assocMult _ (Branch2 MScalarProduct l (Branch2 MScalarProduct c r)) = Just (Branch2 MScalarProduct (Branch2 MScalarProduct l c) r)
assocMult _ _ = Nothing

-- (AC + BC) -> (A+B)C
commonFactorRight :: Rule
commonFactorRight _ (Branch2 MSum (Branch2 MProduct l1 l2) (Branch2 MProduct r1 r2)) =
  if (l2 == r2)
     then Just (Branch2 MProduct (Branch2 MSum l1 r1) l2)
     else Nothing
commonFactorRight _ (Branch2 MSum (Branch2 MScalarProduct l1 l2) (Branch2 MScalarProduct r1 r2)) =
  if (l2 == r2)
     then Just (Branch2 MScalarProduct (Branch2 MSum l1 r1) l2)
     else Nothing
commonFactorRight _ _ = Nothing

-- (AB + AC) -> A(B+C)
commonFactorLeft :: Rule
commonFactorLeft _ (Branch2 MSum (Branch2 MProduct l1 l2) (Branch2 MProduct r1 r2)) =
  if (l1 == r1)
     then Just (Branch2 MProduct l1 (Branch2 MSum l2 r2))
     else Nothing
commonFactorLeft _ (Branch2 MSum (Branch2 MScalarProduct l1 l2) (Branch2 MScalarProduct r1 r2)) =
  if (l1 == r1)
     then Just (Branch2 MScalarProduct l1 (Branch2 MSum l2 r2))
     else Nothing
commonFactorLeft _ _ = Nothing

-- A(B+C) -> AB+AC
-- (A+B)C -> AC+BC
distributeMult :: Rule
distributeMult _ (Branch2 MProduct l (Branch2 MSum c r)) = Just (Branch2 MSum (Branch2 MProduct l c) (Branch2 MProduct l r) )
distributeMult _ (Branch2 MProduct (Branch2 MSum l c) r) = Just (Branch2 MSum (Branch2 MProduct l r) (Branch2 MProduct c r) )
distributeMult _ (Branch2 MScalarProduct l (Branch2 MSum c r)) = Just (Branch2 MSum (Branch2 MScalarProduct l c) (Branch2 MScalarProduct l r) )
distributeMult _ (Branch2 MScalarProduct (Branch2 MSum l c) r) = Just (Branch2 MSum (Branch2 MScalarProduct l r) (Branch2 MScalarProduct c r) )
distributeMult _ _ = Nothing

literalScalars :: Rule
literalScalars _ (Branch2 MSum (LiteralScalar x) (LiteralScalar y)) = Just (LiteralScalar (x+y))
literalScalars _ (Branch2 MDiff (LiteralScalar x) (LiteralScalar y)) = Just (LiteralScalar (x-y))
literalScalars _ (Branch2 MProduct (LiteralScalar x) (LiteralScalar y)) = Just (LiteralScalar (x*y))
literalScalars _ (Branch2 MScalarProduct (LiteralScalar x) (LiteralScalar y)) = Just (LiteralScalar (x*y))
literalScalars _ (Branch1 (MElementWise MExp) (LiteralScalar x)) = Just (LiteralScalar (exp x))
literalScalars _ (Branch1 (MElementWise MLog) (LiteralScalar x)) = Just (LiteralScalar (log x))
literalScalars _ (Branch1 (MElementWise MReciprocal) (LiteralScalar x)) = Just (LiteralScalar (1.0/x))
literalScalars _ (Branch1 MInverse (LiteralScalar x)) = Just (LiteralScalar (1.0/x))
literalScalars _ (Branch2 MScalarProduct (LiteralScalar (1.0)) a) = Just a
literalScalars _ _ = Nothing

diffToScalar :: Rule
diffToScalar _ (Branch2 MDiff a b) = Just (Branch2 MSum a (Branch2 MScalarProduct (LiteralScalar (-1)) b))
diffToScalar _ (Branch2 MSum a (Branch2 MScalarProduct (LiteralScalar c) b)) = 
             if c == -1
             then Just (Branch2 MDiff a b)
             else if c < 0 
                  then Just (Branch2 MDiff a (Branch2 MScalarProduct (LiteralScalar (-c)) b))
                  else Nothing   
diffToScalar _ (Branch2 MDiff a (Branch2 MScalarProduct (LiteralScalar c) b)) = Just (Branch2 MSum a (Branch2 MScalarProduct (LiteralScalar (-c)) b))
diffToScalar _ _ = Nothing

commuteScalarProduct :: Rule
commuteScalarProduct _ (Branch2 MProduct (Branch2 MScalarProduct a b) c) = Just (Branch2 MProduct b (Branch2 MScalarProduct a c))
commuteScalarProduct _ (Branch2 MProduct a (Branch2 MScalarProduct b c)) = Just (Branch2 MProduct (Branch2 MScalarProduct b a) c)
commuteScalarProduct _ _ = Nothing

assocScalarProduct :: Rule
assocScalarProduct _ (Branch2 MProduct (Branch2 MScalarProduct a b) c) = Just (Branch2 MScalarProduct a (Branch2 MProduct b c))
assocScalarProduct _ (Branch2 MScalarProduct a (Branch2 MProduct b c)) = Just (Branch2 MProduct (Branch2 MScalarProduct a b) c) 
assocScalarProduct _ _ = Nothing

collectTerms :: Rule
collectTerms _ (Branch2 MSum (Branch2 MScalarProduct (LiteralScalar n) a) b) 
             | (a==b) = Just (Branch2 MScalarProduct (LiteralScalar (n+1)) a) 
             | otherwise = Nothing
collectTerms _ (Branch2 MSum a b) 
             | (a==b) = Just (Branch2 MScalarProduct (LiteralScalar 2) a) 
             | otherwise = Nothing
collectTerms _ (Branch2 MDiff a b) 
             | (a==b) = Just (Branch2 MScalarProduct (LiteralScalar 0) a) 
             | otherwise = Nothing
collectTerms _ _ = Nothing

dropZeros :: Rule
dropZeros tbl (Branch2 MScalarProduct (LiteralScalar 0) a) = 
          let Right (Matrix r1 c1 _ ) = treeMatrix a tbl in
              Just (ZeroLeaf r1 c1)
dropZeros _ (Branch2 MSum a (ZeroLeaf n m)) = Just a
dropZeros _ (Branch2 MSum (ZeroLeaf n m) b) = Just b
dropZeros _ (Branch2 MDiff a (ZeroLeaf n m)) = Just a
dropZeros _ (Branch2 MDiff (ZeroLeaf n m) b) = Just (Branch2 MScalarProduct (LiteralScalar (-1)) b)
dropZeros _ _ = Nothing

-- tr(AB) = tr(BA)
commuteTrace :: Rule
commuteTrace tbl (Branch1 MTrace (Branch2 MProduct a b)) = 
             let Right (Matrix r1 c1 _ ) = treeMatrix a tbl
                 Right (Matrix r2 c2 _ ) = treeMatrix b tbl in
                 if (r1==c2) 
                 then Just (Branch1 MTrace (Branch2 MProduct b a))
                 else Nothing
commuteTrace tbl (Branch1 MTrace (Branch3 MTernaryProduct a b c)) = 
             let Right (Matrix r1 c1 _ ) = treeMatrix a tbl
                 Right (Matrix r3 c3 _ ) = treeMatrix c tbl in
                 if (r3==r1) 
                 then Just (Branch1 MTrace (Branch3 MTernaryProduct c a b))
                 else Nothing
commuteTrace _ _ = Nothing

-- tr(c) <-> c for scalar c
dissolveTrace :: Rule
dissolveTrace tbl (Branch1 MTrace a) = let Right (Matrix r c _ ) = treeMatrix a tbl in
                                       if (r==1 && c==1) then Just a else Nothing
-- dissolveTrace tbl c@(Branch2 MProduct a b) = 
--              let Right (Matrix r1 c1 _ ) = treeMatrix c tbl in
--                  if (r1 == 1 && c1 == 1)
--                  then Just (Branch1 MTrace c)
--                  else Nothing
dissolveTrace _ _ = Nothing
 
-- tr(A') <-> tr(A)
transposeTrace :: Rule
transposeTrace _ (Branch1 MTrace (Branch1 MTranspose a)) = Just (Branch1 MTrace a)
transposeTrace _ (Branch1 MTrace a) = Just (Branch1 MTrace (Branch1 MTranspose a))
transposeTrace _ _ = Nothing

-- tr(A+B) <-> tr(A) + tr (B)
-- tr(cA) <-> c*tr(A)
linearTrace :: Rule
linearTrace _ (Branch1 MTrace (Branch2 MSum a b)) = Just (Branch2 MSum (Branch1 MTrace a) (Branch1 MTrace b))
linearTrace tbl (Branch2 MSum (Branch1 MTrace a) (Branch1 MTrace b)) = 
            let Right (Matrix r1 c1 _ ) = treeMatrix a tbl
                Right (Matrix r2 c2 _ ) = treeMatrix b tbl in
                if (r1==r2) && (c1==c2)
                then Just (Branch1 MTrace (Branch2 MSum a b))
                else Nothing
linearTrace _ (Branch1 MTrace (Branch2 MScalarProduct a b)) = Just (Branch2 MScalarProduct (Branch1 MTrace a) (Branch1 MTrace b) )
linearTrace tbl (Branch2 MScalarProduct (Branch1 MTrace a) (Branch1 MTrace b) ) = Just (Branch1 MTrace (Branch2 MScalarProduct a b)) 
linearTrace tbl (Branch2 MScalarProduct a (Branch1 MTrace b) ) = Just (Branch1 MTrace (Branch2 MScalarProduct a b)) 
linearTrace _ (Branch2 MProduct (Branch1 MTrace a) (Branch1 MTrace b) ) = Just (Branch1 MTrace (Branch2 MScalarProduct a b)) 
linearTrace _ _ = Nothing

-- tr(I) = dimension
-- det(I) = 1
identityOps :: Rule
identityOps _ (Branch1 MTrace (IdentityLeaf n)) = Just (LiteralScalar (fromIntegral n))
identityOps _ (Branch1 MDet (IdentityLeaf n)) = Just (LiteralScalar 1)
identityOps _ _ = Nothing

traceDiag :: Rule
traceDiag _ (Branch1 MTrace (Branch1 MDiagVM v)) = Just (Branch1 MEntrySum v)
traceDiag _ _ = Nothing

-- det(AB) <-> det(A) * det(B)
factorDet :: Rule
factorDet tbl (Branch1 MDet (Branch2 MProduct a b)) = 
          let Right (Matrix l1 r1 _) = treeMatrix a tbl in
              if (l1==r1) then Just (Branch2 MScalarProduct (Branch1 MDet a) (Branch1 MDet b))
              else Nothing
factorDet tbl (Branch2 MScalarProduct (Branch1 MDet a) (Branch1 MDet b)) = 
          let Right (Matrix l1 r1 _) = treeMatrix a tbl
              Right (Matrix l2 r2 _) = treeMatrix b tbl in
          if (r1 == l2) then Just (Branch1 MDet (Branch2 MProduct a b)) 
          else Nothing
factorDet tbl (Branch2 MProduct (Branch1 MDet a) (Branch1 MDet b)) = 
          let Right (Matrix l1 r1 _) = treeMatrix a tbl
              Right (Matrix l2 r2 _) = treeMatrix b tbl in
          if (r1 == l2) then Just (Branch1 MDet (Branch2 MProduct a b)) 
          else Nothing
factorDet _ _ = Nothing

-- det(A') = det(A)
-- det(A^-1) = 1/det(A)
-- det(C) = 0 if C is rank-deficient 
detProps :: Rule
detProps _ (Branch1 MDet (Branch1 MTranspose a)) = Just (Branch1 MDet a)
detProps _ (Branch1 MDet (Branch1 MInverse a)) = Just (Branch1 (MElementWise MReciprocal) (Branch1 MDet a))
detProps tbl (Branch1 MDet (Branch2 MProduct a b)) = 
          let Right (Matrix l1 r1 _) = treeMatrix a tbl
              Right (Matrix l2 r2 _) = treeMatrix b tbl in
          if (r1 < l1 && l2 < r2) 
          then Just (LiteralScalar 0.0)
          else Nothing
-- todo: add other properties
detProps _ _ = Nothing

logdet :: Rule
logdet tbl  (Branch1 (MElementWise MLog) (Branch1 MDet  a)) = 
       let Right (Matrix _ _ props) = treeMatrix a tbl in
           if LowerTriangular `elem` props
           then Just (Branch1 MEntrySum (Branch1 (MElementWise MLog) (Branch1 MDiagMV a)))
           else if PosDef `elem` props 
           then Just (Branch2 MScalarProduct (LiteralScalar 2) (Branch1 MEntrySum (Branch1 (MElementWise MLog) (Branch1 MDiagMV (Branch1 MChol a)))))
           else Nothing
-- TODO: add LU decomposition for non-posdef matrices
logdet _ _ = Nothing


-- diag(diag(x)) -> x for vector x
-- diag(diag(A)) -> A for diagonal matrices A
cancelDiag :: Rule
cancelDiag _ (Branch1 MDiagMV (Branch1 MDiagVM a)) = Just a
cancelDiag tbl (Branch1 MDiagVM (Branch1 MDiagMV a)) = 
           let Right (Matrix _ _ props) = treeMatrix a tbl in 
               if Diagonal `elem` props 
               then Just a
               else Nothing
cancelDiag _ _ = Nothing

-- diag(a)*diag(b) = diag(a.*b)
-- diag(a)*B = (a*B.T).T in numpy broadcast syntax
-- A*diag(b) = b*A in numpy broadcast syntax
multDiag :: Rule
multDiag _ (Branch2 MProduct (Branch1 MDiagVM a) (Branch1 MDiagVM b)) = Just (Branch1 MDiagVM (Branch2 MHadamardProduct a b))
multDiag _ (Branch2 MHadamardProduct (Branch1 MDiagVM a) (Branch1 MDiagVM b)) = Just (Branch1 MDiagVM (Branch2 MHadamardProduct a b))
multDiag _ (Branch2 MProduct (Branch1 MDiagVM a) b) = Just (Branch1 MTranspose (Branch2 MColProduct a (Branch1 MTranspose b)))
multDiag _ (Branch2 MProduct a (Branch1 MDiagVM b) ) = Just (Branch2 MColProduct b a)
multDiag _ _ = Nothing

invDiag :: Rule
invDiag _ (Branch1 MInverse (Branch1 MDiagVM a)) = Just (Branch1 MDiagVM (Branch1 (MElementWise MReciprocal) a))
invDiag _ _ = Nothing

-- TODO:
-- determinant of a diagonal matrix



-- exp(log(a)) = a
-- log(exp(a)) = a
-- log(a.*b) <=> log(a) + log(b)
-- exp(a + b) <=> exp(a) * exp(b)
-- exp(-a) <=> 1.0/exp(a)
elementUnOps :: Rule
elementUnOps _ (Branch1 (MElementWise MExp) (Branch1 (MElementWise MLog) a)) = Just a
elementUnOps _ (Branch1 (MElementWise MLog) (Branch1 (MElementWise MExp) a)) = Just a
elementUnOps _ (Branch1 (MElementWise MLog) (Branch2 MHadamardProduct a b)) = Just (Branch2 MSum (Branch1 (MElementWise MLog) a) (Branch1 (MElementWise MLog) b))
elementUnOps tbl (Branch1 (MElementWise MLog) (Branch2 MScalarProduct a b)) = 
               let Right (Matrix r1 c1 _) = treeMatrix b tbl in
               if (r1==1) && (c1==1) 
               then Just (Branch2 MSum (Branch1 (MElementWise MLog) a) (Branch1 (MElementWise MLog) b))
               else Nothing
elementUnOps _ (Branch2 MSum (Branch1 (MElementWise MLog) a) (Branch1 (MElementWise MLog) b)) = Just (Branch1 (MElementWise MLog) (Branch2 MHadamardProduct a b))
elementUnOps _ (Branch1 (MElementWise MExp) (Branch2 MSum a b)) = Just (Branch2 MHadamardProduct (Branch1 (MElementWise MExp) a) (Branch1 (MElementWise MExp) b))
elementUnOps _ (Branch2 MHadamardProduct (Branch1 (MElementWise MExp) a) (Branch1 (MElementWise MExp) b)) = Just (Branch1 (MElementWise MExp) (Branch2 MSum a b))
elementUnOps _ (Branch1 (MElementWise MExp) (Branch2 MScalarProduct (LiteralScalar c) a)) = Just (Branch1 (MElementWise MReciprocal) (Branch1 (MElementWise MExp) (Branch2 MScalarProduct (LiteralScalar (-c)) a)))
elementUnOps _ (Branch1 (MElementWise MReciprocal) (Branch1 (MElementWise MExp) (Branch2 MScalarProduct (LiteralScalar c) a))) = Just (Branch1 (MElementWise MExp) (Branch2 MScalarProduct (LiteralScalar (-c)) a))
elementUnOps _ _ = Nothing

-- tr(AB') = sum(A*B)
traceProduct :: Rule
traceProduct tbl (Branch1 MTrace (Branch2 MProduct a (Branch1 MTranspose b))) = 
           let Right (Matrix r1 c1 _) = treeMatrix a tbl
               Right (Matrix r2 c2 _) = treeMatrix b tbl in 
               if (r1==r2) && (c1==c2) 
               then Just (Branch1 MEntrySum (Branch2 MHadamardProduct a b))
               else Nothing
traceProduct tbl (Branch1 MEntrySum (Branch2 MHadamardProduct a b)) = Just (Branch1 MTrace (Branch2 MProduct a (Branch1 MTranspose b)))
traceProduct _ _ = Nothing

entrySumLinear :: Rule
entrySumLinear _ (Branch1 MEntrySum (Branch2 MSum a b)) = Just (Branch2 MSum (Branch1 MEntrySum a) (Branch1 MEntrySum b))
entrySumLinear _  (Branch2 MSum (Branch1 MEntrySum a) (Branch1 MEntrySum b)) = Just (Branch1 MEntrySum (Branch2 MSum a b))
entrySumLinear _ (Branch1 MEntrySum (Branch2 MScalarProduct a b))  = Just (Branch2 MScalarProduct a (Branch1 MEntrySum b))
entrySumLinear _ _ = Nothing

hadamardProductAssoc :: Rule
hadamardProductAssoc _ (Branch2 MHadamardProduct a (Branch2 MHadamardProduct b c)) = Just (Branch2 MHadamardProduct (Branch2 MHadamardProduct a b) c)
hadamardProductAssoc _ (Branch2 MHadamardProduct (Branch2 MHadamardProduct a b) c) = Just (Branch2 MHadamardProduct a (Branch2 MHadamardProduct b c))
hadamardProductAssoc _ _ = Nothing

hadamardProductDist :: Rule
hadamardProductDist _ (Branch2 MHadamardProduct a (Branch2 MSum b c)) = Just (Branch2 MSum (Branch2 MHadamardProduct a b) (Branch2 MHadamardProduct a c))
hadamardProductDist _ (Branch2 MSum (Branch2 MHadamardProduct a b) (Branch2 MHadamardProduct a2 c)) = 
                    if a==a2 then Just (Branch2 MHadamardProduct a (Branch2 MSum b c))
                    else Nothing
hadamardProductDist _ _ = Nothing

hadamardProductCommute :: Rule
hadamardProductCommute _ (Branch2 MHadamardProduct a b) = Just (Branch2 MHadamardProduct b a)
hadamardProductCommute _ _ = Nothing


-- A^-1 B -> A\B
invToLinsolve :: Rule
invToLinsolve tbl (Branch2 MProduct (Branch1 MInverse l) r) =
        let Right (Matrix _ _ props) = treeMatrix l tbl in
            if LowerTriangular `elem` props
            then Just (Branch2 MTriSolve l r)
            else if PosDef `elem` props
            then Just (Branch2 MCholSolve (Branch1 MChol l) r)
            else Just (Branch2 MLinSolve l r)
invToLinsolve tbl (Branch1 MInverse l) = 
        let Right (Matrix r _ props) = treeMatrix l tbl in
            if LowerTriangular `elem` props
            then Just (Branch2 MTriSolve l (IdentityLeaf r))
            else Nothing
invToLinsolve _ _ = Nothing

-- y' (K\y) -> (Ly)'(Ly) where L is the cholesky decomp. of K
-- this should be provable from other rules, but including it explicitly 
-- seems to help escape local minima.
cholSolvetoTri :: Rule
cholSolvetoTri _ (Branch2 MProduct (Branch1 MTranspose a) (Branch2 MCholSolve b c)) = 
               if (a==c)
               then Just (Branch2 MProduct (Branch1 MTranspose (Branch2 MTriSolve b c)) (Branch2 MTriSolve b c))
               else Nothing
cholSolvetoTri _ _ = Nothing

-- HACK to apply Cholesky decomposition to the inverses of leaf nodes.
-- we could instead allow replacing *all* posdef expressions by their 
-- Cholesky decompositions, but then we have to compute properties for
-- every subexpression. Limiting to leaves seems more efficient for now.
invToCholInv :: Rule
invToCholInv tbl (Branch1 MInverse l@(Leaf _)) =
      let Right (Matrix _ _ props) = treeMatrix l tbl in
          if PosDef `elem` props
          then Just (Branch1 MInverse (Branch2 MProduct (Branch1 MChol l) (Branch1 MTranspose (Branch1 MChol l) )))
          else Nothing
invToCholInv _ _ = Nothing

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

-- I^-1 = I
-- I' = I
invariantIdentity :: Rule
invariantIdentity _ (Branch1 MInverse (IdentityLeaf n)) = Just (IdentityLeaf n)
invariantIdentity _ (Branch1 MTranspose (IdentityLeaf n)) = Just (IdentityLeaf n)
invariantIdentity _ _ = Nothing

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
-- (cA)' -> c A' for scalar c
factorTranspose :: Rule
factorTranspose _ (Branch2 MProduct (Branch1 MTranspose r) (Branch1 MTranspose l)) =
        Just (Branch1 MTranspose (Branch2 MProduct l r))
factorTranspose _ (Branch1 MTranspose (Branch2 MScalarProduct a b)) = Just (Branch2 MScalarProduct a (Branch1 MTranspose b))
factorTranspose _ (Branch2 MSum (Branch1 MTranspose a) (Branch1 MTranspose b)) = Just (Branch1 MTranspose (Branch2 MSum a b))
factorTranspose _ _ = Nothing

-- (A'B) -> (B'A)'
-- (this rule is its own inverse when combined with cancelDoubleTranspose)
swapTranspose :: Rule
swapTranspose _ (Branch2 MProduct (Branch1 MTranspose l) r) = Just (Branch1 MTranspose (Branch2 MProduct (Branch1 MTranspose r) l))
swapTranspose _ _ = Nothing

-- A'' = A
-- A' = A for symmetric matrices
cancelTranspose :: Rule
cancelTranspose _ (Branch1 MTranspose (Branch1 MTranspose t)) = Just t
cancelTranspose tbl (Branch1 MTranspose t) = 
                let Right (Matrix _ _ props) = treeMatrix t tbl in
                    if Symmetric `elem` props then Just t else Nothing
cancelTranspose _ _ = Nothing

-- expensive
introduceTranspose :: Rule
introduceTranspose tbl a = let Right (Matrix _ _ props) = treeMatrix a tbl in
                               if Symmetric `elem` props then Just (Branch1 MTranspose a) else Nothing


-- A B' -> (BA)' if A is symmetric.  
-- We could get this ability with a
-- simple rule that allows replacing A with A', but it's expensive to
-- rewrite all symmetric matrices without first knowing whether
-- the rewrite might be useful.
groupTranspose :: Rule
groupTranspose tbl (Branch2 MProduct a (Branch1 MTranspose b)) = 
               let Right (Matrix _ _ props) = treeMatrix a tbl in
               if Symmetric `elem` props
               then Just (Branch1 MTranspose (Branch2 MProduct b a))
               else Nothing
groupTranspose tbl (Branch2 MProduct (Branch1 MTranspose a) b) = 
               let Right (Matrix _ _ props) = treeMatrix b tbl in
               if Symmetric `elem` props
               then Just (Branch1 MTranspose (Branch2 MProduct b a))
               else Nothing
groupTranspose _ _ =  Nothing


symTranspose :: Rule
symTranspose tbl (Branch1 MTranspose a) = Nothing
symTranspose tbl a = let Right (Matrix _ _ props) = treeMatrix a tbl in
                     if Symmetric `elem` props
                     then Just (Branch1 MTranspose a)
                     else Nothing

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
-- (A+C)^-1 -> A^-1 - A^-1 (C^-1 + A^-1)^-1 A^-1
matrixInvLemmaLeft :: Rule
matrixInvLemmaLeft _ (Branch1 MInverse (Branch2 MSum a (Branch3 MTernaryProduct u c v))) =
  Just (Branch2 MDiff (Branch1 MInverse a) (Branch3 MTernaryProduct (Branch2 MProduct (Branch1 MInverse a) u ) (Branch1 MInverse ( Branch2 MSum (Branch1 MInverse c) (Branch3 MTernaryProduct v (Branch1 MInverse a) u) ) ) (Branch2 MProduct v (Branch1 MInverse a)) ))
matrixInvLemmaLeft _ (Branch1 MInverse (Branch2 MSum a c)) =
  Just (Branch2 MDiff (Branch1 MInverse a) (Branch3 MTernaryProduct (Branch1 MInverse a) (Branch1 MInverse ( Branch2 MSum (Branch1 MInverse c) (Branch1 MInverse a) ) ) (Branch1 MInverse a)))
matrixInvLemmaLeft _ _ = Nothing

--  A^-1 - A^-1 U (C^-1 + V A^-1 U)^-1 V A^-1 -> (A+UCV)^-1
matrixInvLemmaRight :: Rule
matrixInvLemmaRight tbl (Branch2 MDiff (Branch1 MInverse a) (Branch3 MTernaryProduct (Branch2 MProduct (Branch1 MInverse b) c ) (Branch1 MInverse ( Branch2 MSum (Branch1 MInverse d) (Branch3 MTernaryProduct e (Branch1 MInverse f) g) )) (Branch2 MProduct h (Branch1 MInverse i)) )) =
                    if (a == b && a == f && a == i)&& (c == g)&& (e == h) then
                    -- MIL A: (A, B, F, I)
                    -- MIL U: C, G
                    -- MIL C: D
                    -- MIL V: E, H
                    Just (Branch1 MInverse (Branch2 MSum a (Branch3 MTernaryProduct c d e)))    
                    else Nothing

matrixInvLemmaRight tbl z@(Branch2 MDiff ainv (Branch3 MTernaryProduct (Branch2 MProduct ainv2 u ) (Branch1 MInverse ( Branch2 MSum cinv (Branch3 MTernaryProduct v ainv3 u2) ) ) (Branch2 MProduct v2 ainv4) ) )  =
   if (ainv == ainv2 && ainv == ainv3 && ainv == ainv4)&& (u == u2)&& (v == v2)
   then Just (Branch1 MInverse (Branch2 MSum (Branch1 MInverse ainv) (Branch3 MTernaryProduct u (Branch1 MInverse cinv) v)))
  else Nothing
matrixInvLemmaRight _ _ = Nothing