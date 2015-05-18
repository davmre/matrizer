module TestCases where

import Language.Haskell.TH
import Import

readTest tfile =  do s <- runIO $ readFile ("../tests/" ++ tfile)
                     return $ LitE $ stringL s
