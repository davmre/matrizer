module Matrizer.Parsing
( readInput
) where

import Control.Applicative hiding ((<|>), many, optional)
import Data.Maybe
import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Control.Monad
import Control.Monad.Error
import Data.Functor.Identity

import Matrizer.MTypes

linespaces :: Parser ()
linespaces = skipMany $ oneOf " \t"

----------------------------------------------------------------------------------------------
-- Lexer
----------------------------------------------------------------------------------------------

token_def = emptyDef{ commentStart = "\"\"\""
                    , commentEnd = "\"\"\""
                    , commentLine = "#"
                    , identStart = letter
                    , identLetter = alphaNum
                    , opStart = oneOf "+-*'^"
                    , opLetter = oneOf "+-*'^1"
                    , reservedOpNames = ["+", "-", "*", "^-1", "'", "\\"]
                    }
TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser token_def


-----------------------------------------------------------------
-- Expression Parser
----------------------------------------------------------------


table :: OperatorTable String u Identity Expr
table = [ [Prefix (m_reservedOp "tr" >> return (Branch1 MTrace))] 
        , [Prefix (m_reservedOp "det" >> return (Branch1 MDet))] 
        , [Prefix (m_reservedOp "sum" >> return (Branch1 MEntrySum))] 
        , [Prefix (m_reservedOp "diag" >> return (Branch1 MDiagMV))] -- guess which diag is meant and fix in preprocessing analysis 
        , [Prefix (m_reservedOp "exp" >> return (Branch1 (MElementWise MExp)))] 
        , [Prefix (m_reservedOp "log" >> return (Branch1 (MElementWise MLog)))] 
        , [Prefix (m_reservedOp "recip" >> return (Branch1 (MElementWise MReciprocal)))] 
        , [Prefix (m_reservedOp "chol" >> return (Branch1 MChol))] 
        , [Postfix (m_reservedOp "^-1" >> return (Branch1 MInverse))]
        , [Postfix (m_reservedOp "'" >> return (Branch1 MTranspose))]
        , [Infix (m_reservedOp "*" >> return (Branch2 MProduct)) AssocLeft]
        , [Infix (m_reservedOp ".*" >> return (Branch2 MHadamardProduct)) AssocLeft]
        , [Infix (m_reservedOp "" >> return (Branch2 MProduct)) AssocLeft]
        , [Infix (m_reservedOp "\\cholsolve" >> return (Branch2 MCholSolve)) AssocLeft]
        , [Infix (m_reservedOp "\\" >> return (Branch2 MLinSolve)) AssocLeft]
        , [Infix (m_reservedOp "+" >> return (Branch2 MSum)) AssocLeft]
        , [Infix (m_reservedOp "-" >> return (Branch2 MDiff)) AssocLeft]
        ]


number = many1 digit
minus = (:) <$> char '-' <*> number
minteger = minus <|> number

mfloat :: Parser Float
mfloat = fmap rd $ (++) <$> minteger <*> decimal
         where rd      = read :: String -> Float
               decimal = option "" $ (:) <$> char '.' <*> number

term :: Parser Expr
term = deriv <|> m_parens exprparser <|> matrix <|> scalar 
       where matrix = m_identifier >>= return . Leaf
             scalar = mfloat >>= return . LiteralScalar

deriv = do m_reservedOp "deriv("
           expr1 <- exprparser
           char ','
           linespaces
           x <- m_identifier
           char ')'
           return (Branch1 (MDeriv x) expr1)

exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"


-----------------------------------------------------
-- Statement Parser
-----------------------------------------------------

data ParserStmt = Assign VarName Expr Bool
instance Show ParserStmt where
    show (Assign v rhs tmp) = show v ++ " := " ++ show rhs ++ if tmp then " #tmp" else ""


mainparser :: Parser Expr
mainparser = m_whiteSpace >> programparser <* eof


buildLetExpr :: [ParserStmt] -> Expr
buildLetExpr ((Assign v rhs tmp) : []) = Let v rhs tmp (Leaf v)
buildLetExpr ((Assign v rhs tmp) :xs) = Let v rhs tmp (buildLetExpr xs)

programparser :: Parser Expr
programparser = do stmts <- m_semiSep1 stmt1
                   let goodStmts = catMaybes stmts
                   return $ buildLetExpr goodStmts

stmt1 :: Parser (Maybe ParserStmt)
stmt1 = do tmpStr <- try (string "tmp") <|> string ""
           linespaces
           v <- m_identifier
           m_reservedOp "="
           linespaces
           e <- exprparser
           return $ Just (Assign v e (tmpStr == "tmp"))
        <|> do m_whiteSpace
               return Nothing

------------------------------------------------------------
-- Parsing Code for the preamble / symbol table
------------------------------------------------------------

------------------------------
-- # Symbol table
-- # Symbol : Size Expression
-- A: n x n
-- B: A x n
-- x: n x 1
-- n ~ 100
-- # Program
-- ABx
--
------------------------------

parseMProp :: Parser MProperty
parseMProp = do propName <- many1 letter
                return $ case propName of
                            "symmetric" -> Symmetric
                            "sym" -> Symmetric
                            "posdef" -> PosDef
                            "pd" -> PosDef
                            "diag" -> Diagonal
                            "ltri" -> LowerTriangular
                            
parsePropList :: Parser [MProperty]
parsePropList = sepBy parseMProp $ many1 $ oneOf " \t,"

parseMatrix :: Parser PreambleLine
parseMatrix = do linespaces
                 v <- m_identifier
                 _ <- char ':'
                 linespaces
                 sym1 <- ((liftM (:[]) letter) <|> many1 digit)
                 linespaces
                 _ <- char 'x'
                 linespaces
                 sym2 <- ((liftM (:[]) letter) <|> many1 digit)
                 linespaces
                 propList <- parsePropList
                 return $ MatrixLine v (MatrixSym sym1 sym2 propList)

parseSymbolDef :: Parser PreambleLine
parseSymbolDef = do linespaces
                    c <- letter
                    linespaces
                    _ <- oneOf "~"
                    linespaces
                    n <- liftM read $ many1 digit
                    return $ SymbolLine c n

parseComment :: Parser String
parseComment = char '#' >> many (noneOf "\r\n")

parseBlankLine :: Parser PreambleLine
parseBlankLine = do linespaces
                    optional parseComment
                    return BlankLine


parsePreamble :: Parser [PreambleLine]
parsePreamble = endBy (try parseSymbolDef
                   <|> try parseMatrix
                   <|> parseBlankLine
                   ) endOfLine

parseInput :: Parser ([PreambleLine], Expr)
parseInput = do lns <- parsePreamble
                spaces
                prgm <- mainparser
                m_whiteSpace
                return (lns, prgm)

--------------------------------------------------------------------------------------
-- Symbol table evaluation: ground out all variable sizes into literal
-- sizes (i.e. convert MatrixSym to Matrix).
--------------------------------------------------------------------------------------

subSymbolDefMatrix :: Map.Map Char Int -> (VarName, MatrixSym) -> ThrowsError (VarName, (Matrix, Maybe Expr))
subSymbolDefMatrix defs (c, (MatrixSym sym1 sym2 propList)) =
    do n1 <- subSymbolDef sym1 defs
       n2 <- subSymbolDef sym2 defs
       return (c, (Matrix n1 n2 (inferProps propList), Nothing) )

inferProps :: [MProperty] -> [MProperty]
inferProps propList = if (PosDef `elem` propList) && (not $ Symmetric `elem` propList)
                      then Symmetric:propList
                      else propList

-- TODO: Gaping holes in the pattern match of this function
subSymbolDef :: String -> Map.Map Char Int -> ThrowsError Int
subSymbolDef s defs =
    let c = (s !! 0)
    in case reads s of
        [(n, "")] -> return n
        [(_, _) ] -> throwError $ BadDimension s
        [       ] -> maybe (throwError $ BadDimension [c])
                           return (Map.lookup c defs)
        _         -> throwError $ BadDimension s


subPreamble :: [PreambleLine] -> ThrowsError SymbolTable
subPreamble preamble =
        let matrices = [(c,n) | (MatrixLine c n)  <- preamble]
            defs = Map.fromList [(c,n) | (SymbolLine c n) <- preamble]
            mapped = mapM (subSymbolDefMatrix defs) matrices
        in (liftM Map.fromList) mapped

readInput :: String -> ThrowsError (SymbolTable, Expr)
readInput s = do (ls, prgm) <- readOrThrow parseInput s
                 tbl <- subPreamble ls
                 return (tbl, prgm)

-----------------------------------------------------------------
-- Test/debugging stuff
-----------------------------------------------------------------

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "matrizer" input of
         Left err -> throwError $ Parser err
         Right val -> return val

printEither :: (Show a, Show b) => Either a b -> IO ()
printEither = either print print

play :: String -> IO ()
play = printEither . readInput

playPreamble :: String -> IO ()
playPreamble = printEither . readOrThrow parsePreamble

playFile :: String -> IO()
playFile fname = readFile fname >>= play
