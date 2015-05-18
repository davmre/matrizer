module Handler.Optimize where

import Matrizer.Util
import Matrizer.Optimization
import Matrizer.CodeGen
import System.Timeout
import Import
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async

limited :: Int -> (a -> b) -> a -> IO (Maybe b)
limited n f a = isLeft <$> race (return $! f a) (threadDelay n)
  where isLeft (Left a) = Just a
        isLeft _        = Nothing


postOptimizeR :: Handler Value
postOptimizeR = 
          do postedText <- runInputPost $ ireq textField "program"
             iters <- runInputPost $ ireq intField "iters"
             beamSize <- runInputPost $ ireq intField "beamSize"
             nRewrites <- runInputPost $ ireq intField "nRewrites"
	     let postedString = (unpack postedText)
	     stuff <-lift $ limited 10000000 (doOptimize postedString  iters beamSize) nRewrites
             case stuff of
                  Nothing -> sendResponseStatus status500 $ object [("error", "optimization timed out after 10 seconds.")]
                  Just (Left err) -> sendResponseStatus status400 $ object [("error", String $ pack $ show err)]
                  Just (Right (tree, flops)) -> return $ object [("prgm", String $ pack $ show tree), 
                                                          ("python", String $ pack $ generateNumpy tree),
                                                          ("matlab", String $ pack $ generateMatlab tree),
                                                          ("flops", Number $ fromIntegral flops)]

