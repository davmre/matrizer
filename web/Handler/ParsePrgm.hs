{-# LANGUAGE OverloadedStrings #-}
module Handler.ParsePrgm where

import Import
import Matrizer.Util
import Matrizer.MTypes
import Yesod.Core.Json

postParsePrgmR :: Handler Value
postParsePrgmR = 
          do postedText <- runInputPost $ ireq textField "program"
	     $(logOther "Submission") $ "processing text: \n" ++ postedText ++ "\n"
             case doParse (unpack postedText) of
                  Left err -> sendResponseStatus status400 $ object [("error", String $ pack $ show err)]
                  Right (tbl, tree, flops) -> return $ object [("symboltable", String $ pack $ show tbl), ("prgm", String $ pack $ pprint tree), ("flops", String $ pack $ showFLOPs flops )]
                  where showFLOPs (Just f) = show f
                        showFLOPs Nothing = "abstract"

