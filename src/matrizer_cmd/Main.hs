module Main where

import Matrizer.Util
import Tests 
import System.Environment

---------------------------------------------------------------


main :: IO ()
main = do args <- getArgs
          if (head args) == "test"
          then runTests
          else if (head args) == "genpython"
          then writePythonTests
          else if (head args) == "debug"
          then runFileDebug $ head $ tail args
          else runFile $ head args

runFile :: String -> IO ()
runFile infile = do inp <- readFile infile
                    putStrLn $ optimizeStr inp


runFileDebug :: String -> IO ()
runFileDebug infile = do inp <- readFile infile
                         case doParse inp of 
                           (Left err) -> putStrLn (show err)
                           (Right (tbl, prgm, flops)) -> runDebug tbl prgm                  

                     
                                           