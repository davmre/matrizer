module Tests where

import Matrizer.MTypes
import Matrizer.Parsing
import Matrizer.Analysis
import Matrizer.Preprocess
import Matrizer.Optimization
import Matrizer.CodeGen
import Matrizer.Equivalence

import Control.Monad
import System.Directory
import System.FilePath
import Data.List 
import qualified Data.Set as Set

testdir = "tests"
pythonTestdir = (combine testdir "gen_python")
solution_suffix = "_soln"

-- have a list of tests and a list of files
-- each call looks at the first file. if it is a matching test (second file), add it to the tests list and remove both. otherwise, ignore it.

runTests :: IO ()
runTests = do files <- getDirectoryContents testdir
              let testFiles = extractTests (Set.fromList files) []
              void $ mapM runTestFile testFiles

writePythonTests :: IO ()
writePythonTests = do files <- getDirectoryContents testdir
                      let testFiles = extractTests (Set.fromList files) []
                      void $ mapM writePythonTest testFiles

extractTests :: Set.Set String -> [String] -> [String]                  
extractTests files tests
             | Set.null files = tests
             | otherwise = let f1 = Set.findMin files
                               f2 = (f1 ++ solution_suffix)
                               nfiles = Set.delete f1 files in
                               if Set.member f2 nfiles
                               then extractTests (Set.delete f2 nfiles) (tests ++ [f1])
                               else extractTests nfiles tests

runTestFile :: String -> IO ()
runTestFile fname = do (tbl, test_tree, soln_tree) <- readTest fname
                       case runTest (tbl, test_tree, soln_tree) of
                            (Right (f1, f2, f3, equiv)) -> putStrLn $ fname ++ ": naive " ++ show f1 ++ " target " ++ show f2 ++ " optimized " ++ show f3 ++ (equivStr equiv) ++ (if (f3 < f2) then " WIN!" else if (f3 == f2) then " PASS" else " FAIL*******")
                            (Left err) -> putStrLn (fname ++ ": " ++ (show err) ++ " ERROR***")
                    where equivStr (Just True) = ""
                          equivStr (Just False) = " NOT EQUIVALENT! " 
                          equivStr Nothing = " COULD NOT DETERMINE EQUIVALENCE! " 

type OptimizerTest = (SymbolTable, Expr, Expr)
readTest :: String -> IO OptimizerTest
readTest fname = do test <- readFile (combine testdir fname)
                    soln <- readFile (combine testdir (fname ++ solution_suffix))
                    let t1 = readInput test
                        t2 = readInput soln
                    case (t1, t2) of
                         (Right (tbl, test_tree),  Right (_, soln_tree)) -> return (tbl, test_tree, soln_tree)
                         (Left test_err, _) -> error (fname ++ ": " ++ show test_err ++ " ERROR***")
                         (_, Left soln_err) -> error (fname ++ ": " ++ show soln_err ++ " ERROR***")

runTest :: OptimizerTest -> ThrowsError (Int, Int, Int, Maybe Bool)
runTest (tbl, t1, t2) = do st1 <- preprocess t1 tbl
                           st2 <- preprocess t2 tbl
                           m1 <- typeCheck st1 tbl
                           m2 <- typeCheck st2 tbl
                           naive_flops <- treeFLOPs st1 tbl
                           soln_flops <- treeFLOPs st2 tbl
                           (opt, opt_flops) <- optimize st1 tbl
                           return $ (naive_flops, soln_flops, naive_flops + opt_flops, testEquivalence tbl opt st2)

writePythonTest :: String -> IO ()
writePythonTest  testname = do
                (tbl, t1, _) <- readTest testname
                let Right naive = preprocess t1 tbl
                    Right m1 = typeCheck naive tbl
                    Right (opt, opt_flops) = optimize naive tbl
                    tablestr = generateTableNumpy tbl
                    formatTarget v = "'" ++ v ++ "',"
                    targetstr = "targets = [" ++ (unwords (map formatTarget (targets naive))) ++ "]"
                    s1 = generateNumpy naive
                    s2 = generateNumpy opt 
                    testdir = (combine pythonTestdir (dropExtension testname))
                createDirectoryIfMissing True testdir
                writeFile (combine testdir "table.py") (tablestr ++ "\n" ++ targetstr ++ "\n") 
                writeFile (combine testdir "naive.py") s1
                writeFile (combine testdir "optimized.py") s2
                putStrLn ("wrote python test: " ++ testdir)
