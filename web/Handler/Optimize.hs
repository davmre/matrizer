module Handler.Optimize where


import Matrizer.Util
import Matrizer.Optimization
import Matrizer.CodeGen
import Matrizer.MTypes
import System.CPUTime
import Import
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Data.Scientific

timeItT :: IO a -> IO (Double, a)
timeItT ioa = do
    t1 <- getCPUTime
    a <- ioa
    t2 <- getCPUTime
    let t :: Double
	t = fromIntegral (t2-t1) * 1e-12
    return (t, a)

limited :: Int -> (a -> b) -> a -> IO (Maybe b)
limited n f a = isLeft <$> race (return $! f a) (threadDelay n)
  where isLeft (Left a) = Just a
        isLeft _        = Nothing


htmlOptPath :: BeamNode -> String
htmlOptPath n = let (s, k) = htmlOptPathHelper n in
                     "<table><tr><td>step</td><td>rule</td><td>score</td><td>expression</td></tr>" ++ s ++ "</table>"
 where htmlOptPathHelper  (BeamNode e s d Nothing) = ("", 1)
       htmlOptPathHelper  (BeamNode e s d (Just n)) = 
        let (prevS, k) = htmlOptPathHelper n 
            newS = prevS ++ "<tr><td>" ++ (show k) ++ "</td><td> " ++ d ++ "</td><td>" ++ (show s) ++ "</td><td>" ++ (pprint e) ++ "</td></tr>" in
            (newS, k+1)               


postOptimizeR :: Handler Value
postOptimizeR = 
          do postedText <- runInputPost $ ireq textField "program"
             iters <- runInputPost $ ireq intField "iters"
             beamSize <- runInputPost $ ireq intField "beamSize"
             nRewrites <- runInputPost $ ireq intField "nRewrites"
	     let postedString = (unpack postedText)
	     (execTime, stuff) <-lift $ timeItT $ limited 10000000 (doOptimize postedString  iters beamSize) nRewrites
             case stuff of
                  Nothing -> sendResponseStatus status500 $ object [("error", String $ pack $ "optimization timed out after 10 seconds.")]
                  Just (Left err) -> sendResponseStatus status400 $ object [("error", String $ pack $ show err)]
                  Just (Right (tree, ctree, optTree, flops, optFlops, optNode)) -> return $ \
                       object [("prgm", String $ pack $ pprint optTree), 
                               ("concrete", String $ pack $ pprint ctree),
                               ("needsconcrete", Bool $ ctree /= tree),
                               ("python", String $ pack $ generateNumpy optTree),
                               ("matlab", String $ pack $ generateMatlab optTree),
                               ("optPath", String $ pack $ htmlOptPath optNode), 
                               ("cflops", Number $ fromIntegral flops), 
                               ("optFlops", Number $ fromIntegral optFlops), 
			       ("exectime", Number $ fromFloatDigits execTime)]

