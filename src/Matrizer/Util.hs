module Matrizer.Util 
( optimizeStr,
  doParse,
  doOptimize,
  parseFile
) where

import qualified Data.Map as Map


import Matrizer.MTypes
import Matrizer.Parsing
import Matrizer.Optimization
import Matrizer.Analysis
import Matrizer.CodeGen

---------------------------------------------------------------

fakeSymbols :: SymbolTable
fakeSymbols = Map.fromList [("A", Matrix 1000 1000 []), ("B", Matrix 1000 1000 []), ("x", Matrix 1000 1 [])]

fakeTree :: Expr
fakeTree = Branch2 MProduct (Branch2 MProduct (Leaf "A") (Leaf "B") ) (Leaf "x")

doParse :: String -> ThrowsError (SymbolTable, Expr, Int)
doParse inp = do (tbl, tree) <- readInput inp
                 prgm <- preprocess tree tbl
                 matr <- typeCheck prgm tbl
                 flops <- treeFLOPs prgm tbl
                 return $ (tbl, prgm, flops)

doOptimize :: String -> Int -> Int -> Int -> ThrowsError (Expr, Int)
doOptimize prgm iters beamSize nRewrites = 
           do (tbl, tree, flops) <- doParse prgm
              (optTree, dflops) <- beamSearchWrapper iters beamSize nRewrites tbl tree
              return $ (optTree, (flops+dflops))

dumpInfo :: SymbolTable -> Expr -> ThrowsError String
dumpInfo tbl raw_prgm = do prgm <- preprocess raw_prgm tbl
                           matr <- typeCheck prgm tbl
                           flops <- treeFLOPs prgm tbl
                           (optPrgm, dflops)  <- optimize prgm tbl
                           return $ "Preamble symbol table: " ++ show tbl ++ "\nCode parsed as:\n" ++ show prgm ++ "\nNaive FLOPs required: " ++ show flops ++ "\nNaive code generated:\n" ++ generateNumpy prgm ++ "\n\nOptimized FLOPs required: " ++ show (flops+dflops)  ++"\nOptimized program:\n" ++ show optPrgm ++ "\nOptimized code generated:\n" ++ generateNumpy optPrgm

dumpRaw tbl raw_prgm = do prgm <- preprocess raw_prgm tbl
                          flops <- treeFLOPs prgm tbl      
                          return $ "Preamble symbol table: " ++ show tbl ++ "\nCode parsed as:\n" ++ show prgm ++ "\nNaive FLOPs required: " ++ show flops ++ "\nNaive code generated:\n" ++ generateNumpy prgm

errorStr :: ThrowsError String -> String
errorStr ts = case ts of 
               Left err -> show err
               Right s -> s

optimizeStr :: String -> String
optimizeStr inp =  case readInput inp of
                        Left err -> show err
                        Right (tbl, tree) -> errorStr $ dumpInfo tbl tree

parseFile :: String -> IO (SymbolTable, Expr, Int)
parseFile fname = do contents <- readFile fname
                     let Right (tbl, tree, int) = doParse contents
                     return $ (tbl, tree, int)