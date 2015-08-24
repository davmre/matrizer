module Matrizer.Util 
( optimizeStr,
  doParse,
  doOptimize,
  parseFile,
  runDebug,
  equivCheck
) where

import qualified Data.Map as Map


import Matrizer.MTypes
import Matrizer.Parsing
import Matrizer.Optimization
import Matrizer.RewriteRules
import Matrizer.Analysis
import Matrizer.Preprocess
import Matrizer.CodeGen
import Matrizer.Equivalence
import Control.Monad
---------------------------------------------------------------

fakeSymbols :: SymbolTable
fakeSymbols = Map.fromList [("A", (Matrix 1000 1000 [], Nothing)), ("B", (Matrix 1000 1000 [], Nothing)), ("x", (Matrix 1000 1 [], Nothing))]

fakeTree :: Expr
fakeTree = Branch2 MProduct (Branch2 MProduct (Leaf "A") (Leaf "B") ) (Leaf "x")

doParse :: String -> ThrowsError (SymbolTable, Expr, Maybe Int)
doParse inp = do (tbl, tree) <- readInput inp
                 prgm <- preprocess tree tbl
                 matr <- typeCheck prgm tbl
                 case treeFLOPs prgm tbl of
                      (Right flops) -> return $ (tbl, prgm, Just flops)
                      (Left err) -> return $ (tbl, prgm, Nothing)

showBeam [] = ""
showBeam ((BeamNode expr n _ _):beam) = "**** " ++ (show n) ++ "\n" ++ pprint expr ++ "\n" ++ (showBeam beam)

runDebug :: SymbolTable -> Expr -> IO ()
runDebug tbl prgm = let beams = beamSearchDebug treeFLOPs optimizationRules 5 20 4 tbl [(BeamNode prgm 0 "init" Nothing)] in 
                    case beams of 
                    (Left err) -> putStrLn (show err)
                    (Right bbeams) -> void $ mapM writeBeam (zip [1..(length bbeams)] bbeams)
                       where writeBeam (n, beam) = writeFile ("beam" ++ show n) (showBeam beam)

doOptimize :: String -> Int -> Int -> Int -> ThrowsError (Expr, Expr, Expr, Int, Int, BeamNode)
doOptimize prgm iters beamSize nRewrites = 
           do (tbl, tree, mflops) <- doParse prgm
              ctree  <- makeConcrete tbl tree
              flops <- treeFLOPs ctree tbl
              node <- beamSearchWrapper treeFLOPs iters beamSize nRewrites tbl ctree
              let (BeamNode optTree oflops _ _) = node in
               return $ (tree, ctree, optTree, flops, oflops, node)

loadConcrete :: String -> ThrowsError (SymbolTable, Expr)
loadConcrete str = do (tbl, tree, mflops) <- doParse str
                      ctree  <- makeConcrete tbl tree
                      return $ (tbl, ctree)

-- read two files
-- show what they're both parsed as
equivCheck :: String -> String -> ThrowsError String
equivCheck s1 s2 = do  (tbl1, ctree1) <- loadConcrete s1
                       (tbl2, ctree2) <- loadConcrete s2 
                       let equiv = testEquivalence tbl1 ctree1 ctree2
                       return $ "First concrete:\n" ++ (pprint ctree1) ++ "\nSecond concrete:\n" ++ (pprint ctree2) ++ "\nequivalence check: " ++ (show equiv)

dumpInfo :: SymbolTable -> Expr -> ThrowsError String
dumpInfo tbl raw_prgm = do prgm <- preprocess raw_prgm tbl
                           matr <- typeCheck prgm tbl
                           ctree <- makeConcrete tbl prgm
                           cmatr <- typeCheck ctree tbl
                           flops <- treeFLOPs ctree tbl
                           optimizedBN  <- optimize ctree tbl                           
                           let (BeamNode optPrgm oflops _ _) = optimizedBN in
                            return $ "Preamble symbol table: " ++ show tbl ++ "\nCode parsed as:\n" ++ pprint prgm ++ (if (ctree == prgm) then "" else "\nTransformed to concrete expression: " ++ pprint ctree ++ "\nType comparison: " ++ (show matr) ++ " vs " ++ (show cmatr)) ++ "\nNaive FLOPs required: " ++ show flops ++ "\nNaive code generated:\n" ++ generateNumpy ctree ++ "\n\noptimizations:\n" ++ pprintOptPath  optimizedBN ++ "\n\nOptimized FLOPs required: " ++ show oflops  ++"\nOptimized program:\n" ++ pprint optPrgm ++ "\nOptimized code generated:\n" ++ generateNumpy optPrgm

dumpRaw tbl raw_prgm = do prgm <- preprocess raw_prgm tbl
                          flops <- treeFLOPs prgm tbl      
                          return $ "Preamble symbol table: " ++ show tbl ++ "\nCode parsed as:\n" ++ pprint prgm ++ "\nNaive FLOPs required: " ++ show flops ++ "\nNaive code generated:\n" ++ generateNumpy prgm

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
                     let Right (tbl, tree, mflops) = doParse contents
                     return $ (tbl, tree)