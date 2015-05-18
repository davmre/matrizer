module Handler.Optimize where

import Matrizer.Util
import Matrizer.Optimization
import Matrizer.CodeGen
import Import

postOptimizeR :: Handler Value
postOptimizeR = 
          do postedText <- runInputPost $ ireq textField "program"
             iters <- runInputPost $ ireq intField "iters"
             beamSize <- runInputPost $ ireq intField "beamSize"
             nRewrites <- runInputPost $ ireq intField "nRewrites"
             case doOptimize (unpack postedText) iters beamSize nRewrites of
                  Left err -> sendResponseStatus status400 $ object [("error", String $ pack $ show err)]
                  Right (tree, flops) -> return $ object [("prgm", String $ pack $ show tree), 
                                                          ("python", String $ pack $ generateNumpy tree),
                                                          ("matlab", String $ pack $ generateMatlab tree),
                                                          ("flops", Number $ fromIntegral flops)]

