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
fakeSymbols = Map.fromList [("A", Matrix 1000 1000 []), ("B", Matrix 1000 1000 []), ("x", Matrix 1000 1 [])]

fakeTree :: Expr
fakeTree = Branch2 MProduct (Branch2 MProduct (Leaf "A") (Leaf "B") ) (Leaf "x")

dumpInfo :: SymbolTable -> Expr -> ThrowsError String
dumpInfo tbl raw_prgm = do prgm <- subIdentity raw_prgm tbl
                           flops <- treeFLOPs prgm tbl
                           (optPrgm, dflops)  <- optimize prgm tbl
                           optFlops <- treeFLOPs optPrgm tbl
                           return $ "Preamble symbol table: " ++ show tbl ++ "\nCode parsed as:\n" ++ show prgm ++ "\nNaive FLOPs required: " ++ show flops ++ "\nNaive code generated:\n" ++ generateNumpy prgm ++ "\n\nOptimized flops required: " ++ show optFlops ++ " (pred " ++ show (flops+dflops) ++")\nOptimized program:\n" ++ show optPrgm ++ "\nOptimized code generated:\n" ++ generateNumpy optPrgm

dumpRaw tbl raw_prgm = do prgm <- subIdentity raw_prgm tbl
                          flops <- treeFLOPs prgm tbl      
                          return $ "Preamble symbol table: " ++ show tbl ++ "\nCode parsed as:\n" ++ show prgm ++ "\nNaive FLOPs required: " ++ show flops ++ "\nNaive code generated:\n" ++ generateNumpy prgm

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
            Right (tbl, tree) -> putStrLn $ errorStr $ dumpInfo tbl tree
            