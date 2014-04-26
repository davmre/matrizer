module MTypes where

import qualified Data.Map as Map
import Text.Parsec
import Control.Monad.Error

----------------------------------------------------------------------
-- AST Definition
-----------------------------------------------------------------------

data Stmt = Assign Char Expr 
           | Seq [Stmt]
     deriving (Eq, Ord)

data Expr = Leaf Char
           | Branch1 UnOp Expr
           | Branch2 BinOp Expr Expr
           | Branch3 TernOp Expr Expr Expr
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
    show (Leaf a) = [a]
    show (Branch1 op c) = "(" ++ show op ++ " " ++ show c ++ ")"
    show (Branch2 op a b) = "(" ++ show op ++ " " ++ show a ++ " "
         ++ show b ++ ")"
    show (Branch3 op a b c) = "(" ++ show op ++ " " ++ show a ++ " "
         ++ show b ++ " " ++ show c ++ ")"

instance Show Stmt where
   show (Assign c e) =  c : " := " ++ show e
   show (Seq x) = case x of
                   z:[] -> (show z) 
                   z:zs -> (show z) ++ "\n" ++ (show $ Seq zs)
                   _ -> "ERROR: this should never happen (in Show statement)"
   show (Seq (x:[])) = show x
   show (Seq (x:xs)) = show x ++ "\n" ++ (show $ Seq xs)
   show (Seq []) = ""


------------------------------------------------------------------------
-- Symbol Table Definition
------------------------------------------------------------------------

type SymbolTable = Map.Map Char Matrix
type SizeTable = Map.Map Char Int

data MatrixSym = MatrixSym String String [MProperty]
data Matrix = Matrix Int Int [MProperty]
data MProperty = Symmetric
               | PosDef
               | Diagonal
               deriving (Eq, Enum)
data PreambleLine = MatrixLine Char MatrixSym
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
            | UnboundName Char
            | Default String
            | BadDimension String
            | Parser ParseError

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
showError (UnboundName c)  = "Undefined matrix name " ++ show c
showError (Default s)      = "Default Error???" ++ show s
showError (BadDimension d) = "Invalid dimension specification'" ++ show d ++ "'"
showError (Parser err)     = "Parse error at " ++ show err

instance Show MError where show = showError

instance Error MError where
         noMsg = Default "An error has occurred"
         strMsg = Default

type ThrowsError = Either MError

trapError :: (Show a, MonadError a m) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left err)  = error $ show err

---------------------------------------------------------------------------
