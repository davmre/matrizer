module Matrizer.Util 
( optimizeStr,
  doParse,
  doOptimize,
  parseFile,
  runDebug
) where

import qualified Data.Map as Map


import Matrizer.MTypes
import Matrizer.Parsing
import Matrizer.Optimization
import Matrizer.Analysis
import Matrizer.Preprocess
import Matrizer.CodeGen
import Control.Monad
---------------------------------------------------------------

fakeSymbols :: SymbolTable
fakeSymbols = Map.fromList [("A", Matrix 1000 1000 []), ("B", Matrix 1000 1000 []), ("x", Matrix 1000 1 [])]

fakeTree :: Expr
fakeTree = Branch2 MProduct (Branch2 MProduct (Leaf "A") (Leaf "B") ) (Leaf "x")

doParse :: String -> ThrowsError (SymbolTable, Expr)
doParse inp = do (tbl, tree) <- readInput inp
                 prgm <- preprocess tree tbl
                 matr <- typeCheck prgm tbl
                 return $ (tbl, prgm)

showBeam [] = ""
showBeam ((expr, n):beam) = "**** " ++ (show n) ++ "\n" ++ show expr ++ "\n" ++ (showBeam beam)

runDebug :: SymbolTable -> Expr -> IO ()
runDebug tbl prgm = let beams = beamSearchDebug treeFLOPs 5 20 4 tbl [(prgm, 0)] in 
                    case beams of 
                    (Left err) -> putStrLn (show err)
                    (Right bbeams) -> void $ mapM writeBeam (zip [1..(length bbeams)] bbeams)
                       where writeBeam (n, beam) = writeFile ("beam" ++ show n) (showBeam beam)

doOptimize :: String -> Int -> Int -> Int -> ThrowsError (Expr, Int)
doOptimize prgm iters beamSize nRewrites = 
           do (tbl, tree) <- doParse prgm
              ctree  <- makeConcrete tbl tree
              flops <- treeFLOPs ctree tbl
              (optTree, dflops) <- beamSearchWrapper treeFLOPs iters beamSize nRewrites tbl ctree
              return $ (optTree, (flops+dflops))

dumpInfo :: SymbolTable -> Expr -> ThrowsError String
dumpInfo tbl raw_prgm = do prgm <- preprocess raw_prgm tbl
                           matr <- typeCheck prgm tbl
                           ctree <- makeConcrete tbl prgm
                           cmatr <- typeCheck ctree tbl
                           flops <- treeFLOPs ctree tbl
                           (optPrgm, dflops)  <- optimize ctree tbl                           
                           return $ "Preamble symbol table: " ++ show tbl ++ "\nCode parsed as:\n" ++ show prgm ++ (if (ctree == prgm) then "" else "\nTransformed to concrete expression: " ++ show ctree ++ "\nType comparison: " ++ (show matr) ++ " vs " ++ (show cmatr)) ++ "\nNaive FLOPs required: " ++ show flops ++ "\nNaive code generated:\n" ++ generateNumpy ctree ++ "\n\nOptimized FLOPs required: " ++ show (flops+dflops)  ++"\nOptimized program:\n" ++ show optPrgm ++ "\nOptimized code generated:\n" ++ generateNumpy optPrgm

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

parseFile :: String -> IO (SymbolTable, Expr)
parseFile fname = do contents <- readFile fname
                     let Right (tbl, tree) = doParse contents
                     return $ (tbl, tree)