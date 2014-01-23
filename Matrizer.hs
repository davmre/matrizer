module Main where

import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Error
import System.Environment

import MTypes
import Parsing
import Optimization
import Analysis


-------------------------------------------------------------

-- Code generation for numpy arrays

generateNumpy :: MTree -> String
generateNumpy (Leaf a) = [a]
generateNumpy (Branch2 MProduct t1 t2) = "np.dot(" ++ (generateNumpy t1) ++ 
                                          ", " ++ (generateNumpy t2)  ++ ")" 
generateNumpy (Branch2 MSum t1 t2) = (generateNumpy t1) ++ " + " ++ (generateNumpy t2)
generateNumpy (Branch1 MInverse t) = "np.linalg.inv(" ++ (generateNumpy t) ++ ")"
generateNumpy (Branch1 MTranspose t) = (generateNumpy t) ++ ".T" -- caution: might we need parentheses here?
generateNumpy (Branch1 MNegate t) = "-" ++ (generateNumpy t)

---------------------------------------------------------------

fakeSymbols :: SymbolTable
fakeSymbols = Map.fromList [('A', Matrix 1000 1000 []), ('B', Matrix 1000 1000 []), ('x', Matrix 1000 1 [])]

fakeTree :: MTree
fakeTree = Branch2 MProduct (Branch2 MProduct (Leaf 'A') (Leaf 'B')) (Leaf 'x')

dumpInfo :: SymbolTable -> MTree -> ThrowsError String
dumpInfo tbl tree = do matr <- treeMatrix tree tbl
                       flops <- treeFLOPs tree tbl
                       (optFlops, optTree) <- optimize tree tbl
                       return $ "Symbol table: " ++ show tbl ++ "\nParsed as: " ++ show tree ++ "\nResulting matrix: " ++ show matr ++ "\nNaive FLOPs required: " ++ show flops ++ "\nNaive code generated: " ++ generateNumpy tree ++ "\n\nOptimized flops required: " ++ show optFlops ++ "\nOptimized tree: " ++ show optTree ++ "\nOptimized code: " ++ generateNumpy optTree

errorStr :: ThrowsError String -> String
errorStr ts = case ts of 
               Left err -> show err
               Right s -> s

main = do args <- getArgs
          let infile = head args
          inp <- readFile infile
          case readInput inp of
            Left err -> print err
            Right (tbl, tree) -> putStrLn $ errorStr $ dumpInfo tbl tree
            