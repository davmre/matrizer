module Main where

import Matrizer
import Tests 
import System.Environment

---------------------------------------------------------------


main :: IO ()
main = do args <- getArgs
          if (head args) == "test"
          then runTests
          else if (head args) == "genpython"
          then writePythonTests
          else runFile $ head args

runFile :: String -> IO ()
runFile infile = do inp <- readFile infile
                    putStrLn $ optimizeStr inp
