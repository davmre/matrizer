module Matrizer.MTypes  where

import qualified Data.Map as Map
import Text.Parsec
import Control.Monad.Error

----------------------------------------------------------------------
-- AST Definition
-----------------------------------------------------------------------

type VarName = String

data Expr = Leaf VarName
           | IdentityLeaf Int
           | ZeroLeaf Int Int
           | LiteralScalar Float
           | Branch1 UnOp Expr
           | Branch2 BinOp Expr Expr
           | Branch3 TernOp Expr Expr Expr
           | Let VarName Expr Bool Expr -- bool flag specifies whether this intermediate variable can be optimized out
           deriving (Eq, Ord, Show)

data TernOp = MTernaryProduct deriving (Eq, Ord, Show)
data BinOp = MProduct
           | MSum
           | MDiff
           | MLinSolve
           | MCholSolve
           | MTriSolve
           | MScalarProduct
           | MHadamardProduct
           | MColProduct
           deriving (Eq, Ord, Enum, Show)

data UnOp = MInverse
          | MTranspose
          | MChol
          | MTrace
          | MDet
          | MDeriv VarName
          | MUnresolvedDeriv VarName
          | MDifferential
          | MDiagVM -- convert a vector to a diagonal matrix
          | MDiagMV -- extract a matrix diagonal as a vector
          | MEntrySum
          | MElementWise ScalarOp
          deriving (Eq, Ord, Show)

data ScalarOp = MLog
              | MExp -- TODO: support matrix exponentials
              | MReciprocal
              deriving (Eq, Ord, Enum, Show)

-- AST pretty printing
pprint_ternop MTernaryProduct = "***"

pprint_binop MProduct = "mmul"
pprint_binop MScalarProduct = "smul"
pprint_binop MHadamardProduct = "hmul"
pprint_binop MColProduct = "cmul" -- don't really expect people to use this input syntax 
                            -- except for internal test cases
pprint_binop MSum = "add"
pprint_binop MDiff = "sub"
pprint_binop MLinSolve = "solve"
pprint_binop MTriSolve = "triSolve"
pprint_binop MCholSolve = "cholSolve"


pprint_unop MInverse = "inv"
pprint_unop MTranspose = "transpose"
pprint_unop MChol = "chol"
pprint_unop MTrace = "tr"
pprint_unop (MDeriv v) = "deriv_" ++ v
pprint_unop (MUnresolvedDeriv v) = "unresolved_deriv_" ++ v
pprint_unop MDifferential = "differential"
pprint_unop MDet = "det"
pprint_unop MDiagVM = "toDiag"
pprint_unop MDiagMV = "diag"
pprint_unop MEntrySum = "sum"
pprint_unop (MElementWise sop) = pprint_scalarop sop

pprint_scalarop MLog = "log"
pprint_scalarop MExp = "exp"
pprint_scalarop MReciprocal = "recip"


pprint (Leaf a) = a
pprint (IdentityLeaf _) = "I"
pprint (ZeroLeaf _ _) = "0"
pprint (LiteralScalar x) = show x
pprint (Branch1 op c) = "(" ++ pprint_unop op ++ " " ++ pprint c ++ ")"
pprint (Branch2 op a b) = "(" ++ pprint_binop op ++ " " ++ pprint a ++ " "
         ++ pprint b ++ ")"
pprint (Branch3 op a b c) = "(" ++ pprint_ternop op ++ " " ++ pprint a ++ " "
         ++ pprint b ++ " " ++ pprint c ++ ")"
pprint (Let v a tmp b) = "(let (" ++ v ++ " := " ++ pprint a ++ (if tmp then " #temporary ) "  else ") ") ++ "\n" ++ pprint b ++ ")"



------------------------------------------------------------------------
-- Symbol Table Definition
------------------------------------------------------------------------

type SymbolTable = Map.Map VarName Matrix

data MatrixSym = MatrixSym String String [MProperty]
data Matrix = Matrix Int Int [MProperty]
data MProperty = Symmetric
               | PosDef
               | Diagonal
               | LowerTriangular
               deriving (Eq, Enum)
data PreambleLine = MatrixLine VarName MatrixSym
                  | SymbolLine Char Int
                  | BlankLine
                  deriving (Show)

--------------------------------------------
-- Arjun comment:
-- The parser reads in the symbol table, where each line is a
-- MatrixSym (i.e. "A: n x n" is a MatrixSym Varsize "n" Varsize "n"
-- []). But eventually we want all the Varsizes to be concrete
-- integers, which eventually gets converted to type Matrix. Not sure
-- this is the best way, what about statically checking that the Size
-- constructor is of the "LitSize" type and not the "VarSize" type...?
--------------------------------------------

instance Show MProperty where
    show Symmetric = "symmetric"
    show PosDef = "posdef"
    show Diagonal = "diag"
    show LowerTriangular = "ltri"

instance Show Matrix where
    show (Matrix r c props) = show r ++ "x" ++ show c ++ " " ++ show props

showDim :: Matrix -> String
showDim (Matrix r c _) =  (show r) ++ "x" ++ (show c)

instance Show MatrixSym where
    show (MatrixSym r c props) = show r ++ "x" ++ show c ++ " "
                                 ++ show props


---------------------------------------------------------------------------------------------------
-- Error definitions

-- Datatype for errors --
data MError = SizeMismatch BinOp Matrix Matrix Expr Expr
            | SizeMismatchTern TernOp Matrix Matrix Matrix
            | WrongProperties BinOp [MProperty] [MProperty] Expr Expr
            | WrongProperties1 UnOp [MProperty] Expr
            | InvalidOp UnOp Matrix
            | UnboundName VarName
            | Default String
            | BadDimension String
            | AnalysisError String
            | Parser ParseError
            | BadCrumbs Expr String
            | MaybeError String
            | BadOptimization Expr Expr MError 
            | DerivativeFail Expr VarName
            | AbstractExpression Expr

showError :: MError -> String
showError (SizeMismatch op m1 m2 t1 t2) =
        "Invalid matrix dimensions for operation " ++ show op ++ " ("
        ++ showDim m1 ++ "), (" ++ showDim m2 ++ ")"
        ++ ", applied to trees:\n" ++ (show t1) ++ "\n" ++ (show t2) 
showError (SizeMismatchTern op m1 m2 m3) =
        "Invalid matrix dimensions for ternary operator '"
        ++ show op ++ "' applied to matrices " ++ showDim m1 ++ ", "
        ++ showDim m2 ++ ", " ++ showDim m3
showError (WrongProperties op props1 props2 t1 t2) =
        "Operator '" ++ show op
        ++ "' cannot apply to matrices with properties " ++ show props1
        ++ ", " ++ show props2  ++ ", trees:\n" ++ (show t1) ++ "\n" ++ (show t2)
showError (WrongProperties1 op props t) =
        "Operator '" ++ show op
         ++ "' cannot apply to matrix with properties " ++ show props
          ++ ", " ++ ", trees:\n" ++ (show t) ++ "\n"
showError (InvalidOp op m) =
        "Invalid operation '" ++ show op ++ "' on matrix " ++ show m
showError (UnboundName s)  = "Undefined matrix name " ++ s
showError (Default s)      = "Default Error???" ++ show s
showError (BadDimension d) = "Invalid dimension specification'" ++ show d ++ "'"
showError (Parser err)     = "Parse error at " ++ show err
showError (AnalysisError err)     = "Analysis error: " ++ show err
showError (BadCrumbs exp err)     = "Breadcrumbs don't match the current expression: " ++ show exp ++ ", additional info: " ++ err
showError (MaybeError err)     = "Maybe expression returned Nothing: " ++ show err
showError (BadOptimization t t2 err) = "Optimization rule returned invalid expression.\nOriginal: "++ show t ++ "\nOptimized: " ++ show t2 ++ "\nError: " ++ show err
showError (DerivativeFail expr v) = "Could not differentiate expression: " ++ show expr ++ " with respect to " ++ v
showError (AbstractExpression expr) = "Tried to generate code or compute FLOPs for abstract expression: " ++ show expr
instance Show MError where show = showError

instance Error MError where
         noMsg = Default "An error has occurred"
         strMsg = Default

type ThrowsError = Either MError

maybeToError :: Maybe a -> ThrowsError a
maybeToError (Just val) = return val
maybeToError Nothing = Left $ MaybeError "automatically converted"

trapError :: b -> (a-> b) -> (ThrowsError a) -> b
trapError d f (Right v) = f v
trapError d f (Left _) = d

--trapError :: (Show a, MonadError a m) => m String -> m String
--trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left err)  = error $ show err

---------------------------------------------------------------------------
