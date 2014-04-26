module Main where

import qualified Data.Map as Map
import System.Environment

import MTypes
import Parsing
import Optimization
import Analysis
import CodeGen

---------------------------------------------------------------

fakeSymbols :: SymbolTable
fakeSymbols = Map.fromList [('A', Matrix 1000 1000 []), ('B', Matrix 1000 1000 []), ('x', Matrix 1000 1 [])]

fakeTree :: Expr
fakeTree = Branch2 MProduct (Branch2 MProduct (Leaf 'A') (Leaf 'B')) (Leaf 'x')

dumpExprInfo :: SymbolTable -> Expr -> ThrowsError String
dumpExprInfo tbl tree = do matr <- treeMatrix tree tbl
                           flops <- treeFLOPs tree tbl
                           (optFlops, optTree) <- optimize tree tbl
                           return $ "Symbol table: " ++ show tbl ++ "\nParsed as: " ++ show tree ++ "\nResulting matrix: " ++ show matr ++ "\nNaive FLOPs required: " ++ show flops ++ "\nNaive code generated: " ++ generateNumpy tree ++ "\n\nOptimized flops required: " ++ show optFlops ++ "\nOptimized tree: " ++ show optTree ++ "\nOptimized code: " ++ generateNumpy optTree

dumpProgramInfo :: SymbolTable -> Stmt -> ThrowsError String
--dumpProgramInfo tbl prgm = do flops <- programFLOPs prgm tbl
--                              return $ "Symbol table: " ++ show tbl ++ "\nParsed as: " ++ show prgm ++ "\nNaive FLOPs required: " ++ show flops ++ "\nNaive code generated" ++ generateNumpyStmt prgm
dumpProgramInfo tbl prgm = Right $ "Symbol table: " ++ show tbl ++ "\nParsed as: " ++ show prgm ++ "\nNaive code generated:\n" ++ generateNumpyStmt prgm

errorStr :: ThrowsError String -> String
errorStr ts = case ts of 
               Left err -> show err
               Right s -> s

main :: IO ()
main = do args <- getArgs
          let infile = head args
          inp <- readFile infile
          case readInput inp of
            Left err -> print err
            Right (tbl, tree) -> putStrLn $ errorStr $ dumpProgramInfo tbl tree
            