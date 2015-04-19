module Tests where

import MTypes
import Parsing
import Analysis
import Optimization

import Control.Monad
import System.Directory
import System.FilePath
import Data.List 
import qualified Data.Set as Set

testdir = "tests"
solution_suffix = "_soln"

-- have a list of tests and a list of files
-- each call looks at the first file. if it is a matching test (second file), add it to the tests list and remove both. otherwise, ignore it.

runTests :: IO ()
runTests = do files <- getDirectoryContents testdir
              let testFiles = extractTests (Set.fromList files) []
              void $ mapM runTestFile testFiles

extractTests :: Set.Set String -> [String] -> [String]                  
extractTests files tests
             | Set.null files = tests
             | otherwise = let f1 = Set.findMin files
                               f2 = (f1 ++ solution_suffix)
                               nfiles = Set.delete f1 files in
                               if Set.member f2 nfiles
                               then extractTests (Set.delete f2 nfiles) (f1:tests)
                               else extractTests nfiles tests

runTestFile :: String -> IO ()
runTestFile fname = do (tbl, test_tree, soln_tree) <- readTest fname
                       case runTest (tbl, test_tree, soln_tree) of
                            (Right (f1, f2, f3)) -> putStrLn $ fname ++ ": naive " ++ show f1 ++ " target " ++ show f2 ++ " optimized " ++ show f3 ++ (if (f3 < f2) then " WIN!****" else if (f3 == f2) then " PASS" else "FAIL")
                            (Left err) -> putStrLn (fname ++ ": " ++ (show err))

type OptimizerTest = (SymbolTable, Expr, Expr)
readTest :: String -> IO OptimizerTest
readTest fname = do test <- readFile (combine testdir fname)
                    soln <- readFile (combine testdir (fname ++ solution_suffix))
                    let t1 = readInput test
                        t2 = readInput soln
                    case (t1, t2) of
                         (Right (tbl, test_tree),  Right (_, soln_tree)) -> return (tbl, test_tree, soln_tree)
                         (Left test_err, _) -> error (show test_err)
                         (_, Left soln_err) -> error (show soln_err)

runTest :: OptimizerTest -> ThrowsError (Int, Int, Int)
runTest (tbl, t1, t2) = do st1 <- subIdentity t1 tbl
                           st2 <- subIdentity t2 tbl
                           naive_flops <- treeFLOPs st1 tbl
                           soln_flops <- treeFLOPs st2 tbl
                           (opt, opt_flops) <- optimize st1 tbl
                           return $ (naive_flops, soln_flops, naive_flops + opt_flops)