module MTypes where

import qualified Data.Map as Map
import Text.Parsec
import Control.Monad.Error

----------------------------------------------------------------------
-- AST Definition
-----------------------------------------------------------------------

data MTree = Leaf Char | Branch1 UnOp MTree | Branch2 BinOp MTree MTree deriving (Eq, Ord)
data BinOp = MProduct | MSum | MLinSolve deriving (Eq, Ord) 
data UnOp = MInverse | MTranspose | MNegate deriving (Eq, Ord)




-- AST pretty printing
showBinOp :: BinOp -> String
showBinOp MProduct = "*"
showBinOp MSum = "+"
showBinOp MLinSolve = "\\"
instance Show BinOp where show = showBinOp

showUnOp :: UnOp -> String
showUnOp MInverse = "inv"
showUnOp MTranspose = "transpose"
showUnOp MNegate = "neg"
instance Show UnOp where show = showUnOp

showTree :: MTree -> String
showTree (Leaf a) = [a]
showTree (Branch1 op c) = "(" ++ show op ++ " " ++ showTree c ++ ")"
showTree (Branch2 op a b) = "(" ++ show op ++ " " ++ showTree a ++ " " ++ showTree b ++ ")"
instance Show MTree where show = showTree


------------------------------------------------------------------------
-- Symbol Table Definition
------------------------------------------------------------------------

type SymbolTable = Map.Map Char Matrix
type SizeTable = Map.Map Char Int

data MatrixSym = MatrixSym String String [MProperty]
data Matrix = Matrix Int Int [MProperty] 
data MProperty = Symmetric | PosDef | Diagonal deriving Eq
data PreambleLine = MatrixLine Char MatrixSym | SymbolLine Char Int | BlankLine  deriving (Show)
                                                                                          
--------------------------------------------
-- Arjun comment:
-- The parser reads in the symbol table, where each line is a
-- MatrixSym (i.e. "A: n x n" is a MatrixSym Varsize "n" Varsize "n"
-- []). But eventually we want all the Varsizes to be concrete
-- integers, which eventually gets converted to type Matrix. Not sure
-- this is the best way, what about statically checking that the Size
-- constructor is of the "LitSize" type and not the "VarSize" type...?
--------------------------------------------

showMProperty :: MProperty -> String
showMProperty Symmetric = "symmetric"
showMProperty PosDef = "posdef"
showMProperty Diagonal = "diag"
instance Show MProperty where show = showMProperty

showMatrix (Matrix rows cols props) = (show rows) ++ "x" ++ (show cols) ++ " " ++ (show props)
instance Show Matrix where show = showMatrix

showDim :: Matrix -> String
showDim (Matrix r c props) =  (show r) ++ "x" ++ (show c)

showMatrixSym (MatrixSym rows cols props) = (show rows) ++ "x" ++ (show cols) ++ " " ++ (show props)
instance Show MatrixSym where show = showMatrixSym


---------------------------------------------------------------------------------------------------
-- Error definitions

-- Datatype for errors --
data MError = SizeMismatch BinOp Matrix Matrix 
            | InvalidOp UnOp Matrix
            | UnboundName Char
            | Default String
            | BadDimension String
            | Parser ParseError

showError :: MError -> String
showError (SizeMismatch op m1 m2) = "Invalid matrix dimensions for operation (" ++ showDim m1 ++ ") " ++ show op ++ " (" ++ showDim m2 ++ ")"
showError (InvalidOp op m) = "Invalid operation '" ++ show op ++ "' on matrix " ++ show m 
showError (UnboundName c) = "Undefined matrix name " ++ show c
showError (Default s) = "Default Error???" ++ show s
showError (BadDimension d) = "Invalid dimension specification'" ++ show d ++ "'"
showError (Parser err) = "Parse error at " ++ show err

instance Show MError where show = showError

instance Error MError where
         noMsg = Default "An error has occurred"
         strMsg = Default

type ThrowsError = Either MError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

---------------------------------------------------------------------------
