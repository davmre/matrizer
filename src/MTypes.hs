module MTypes where

import qualified Data.Map as Map
import Text.Parsec
import Control.Monad.Error

----------------------------------------------------------------------
-- AST Definition
-----------------------------------------------------------------------

type VarName = String

data Expr = Leaf VarName
           | IdentityLeaf Int
           | Branch1 UnOp Expr
           | Branch2 BinOp Expr Expr
           | Branch3 TernOp Expr Expr Expr
           | Let VarName Expr Bool Expr -- bool flag specifies whether this intermediate variable can be optimized out
           deriving (Eq, Ord)

data TernOp = MTernaryProduct deriving (Eq, Ord)
data BinOp = MProduct
           | MSum
           | MLinSolve
           | MCholSolve
           deriving (Eq, Ord, Enum)
data UnOp = MInverse
          | MTranspose
          | MNegate
          deriving (Eq, Ord, Enum)

-- AST pretty printing

instance Show TernOp where
    show _ = "***"

instance Show BinOp where
    show MProduct = "*"
    show MSum = "+"
    show MLinSolve = "\\"
    show MCholSolve = "cholSolve"

instance Show UnOp where
    show MInverse = "inv"
    show MTranspose = "transpose"
    show MNegate = "neg"

instance Show Expr where
    show (Leaf a) = a
    show (IdentityLeaf _) = "I"
    show (Branch1 op c) = "(" ++ show op ++ " " ++ show c ++ ")"
    show (Branch2 op a b) = "(" ++ show op ++ " " ++ show a ++ " "
         ++ show b ++ ")"
    show (Branch3 op a b c) = "(" ++ show op ++ " " ++ show a ++ " "
         ++ show b ++ " " ++ show c ++ ")"
    show (Let v a tmp b) = "(let (" ++ v ++ " := " ++ show a ++ (if tmp then " #temporary ) "  else ") ") ++ "\n" ++ show b ++ ")"

------------------------------------------------------------------------
-- Symbol Table Definition
------------------------------------------------------------------------

type SymbolTable = Map.Map VarName Matrix

data MatrixSym = MatrixSym String String [MProperty]
data Matrix = Matrix Int Int [MProperty]
data MProperty = Symmetric
               | PosDef
               | Diagonal
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
data MError = SizeMismatch BinOp Matrix Matrix
            | SizeMismatchTern TernOp Matrix Matrix Matrix
            | WrongProperties BinOp [MProperty] [MProperty]
            | InvalidOp UnOp Matrix
            | UnboundName VarName
            | Default String
            | BadDimension String
            | AnalysisError String
            | Parser ParseError
            | BadCrumbs Expr
            | MaybeError String

showError :: MError -> String
showError (SizeMismatch op m1 m2) =
        "Invalid matrix dimensions for operation ("
        ++ showDim m1 ++ ") " ++ show op ++ " (" ++ showDim m2 ++ ")"
showError (SizeMismatchTern op m1 m2 m3) =
        "Invalid matrix dimensions for ternary operator '"
        ++ show op ++ "' applied to matrices " ++ showDim m1 ++ ", "
        ++ showDim m2 ++ ", " ++ showDim m3
showError (WrongProperties op props1 props2) =
        "Operator '" ++ show op
        ++ "' cannot apply to matrices with properties " ++ show props1
        ++ ", " ++ show props2
showError (InvalidOp op m) =
        "Invalid operation '" ++ show op ++ "' on matrix " ++ show m
showError (UnboundName s)  = "Undefined matrix name " ++ s
showError (Default s)      = "Default Error???" ++ show s
showError (BadDimension d) = "Invalid dimension specification'" ++ show d ++ "'"
showError (Parser err)     = "Parse error at " ++ show err
showError (AnalysisError err)     = "Analysis error: " ++ show err
showError (BadCrumbs exp)     = "Breadcrumbs don't match the current expression: " ++ show exp
showError (MaybeError err)     = "Maybe expression returned Nothing: " ++ show err

instance Show MError where show = showError

instance Error MError where
         noMsg = Default "An error has occurred"
         strMsg = Default

type ThrowsError = Either MError

maybeToError :: Maybe a -> ThrowsError a
maybeToError (Just val) = return val
maybeToError Nothing = Left $ MaybeError "automatically converted"

trapError :: (Show a, MonadError a m) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left err)  = error $ show err

---------------------------------------------------------------------------
