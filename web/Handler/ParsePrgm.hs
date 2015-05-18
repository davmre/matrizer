{-# LANGUAGE OverloadedStrings #-}
module Handler.ParsePrgm where

import Import
import Matrizer.Util
import Yesod.Core.Json

postParsePrgmR :: Handler Value
postParsePrgmR = 
          do postedText <- runInputPost $ ireq textField "program"
             case doParse (unpack postedText) of
                  Left err -> return $ object [("error", String $ pack $ show err)]
                  Right (tbl, tree, flops) -> return $ object [("symboltable", String $ pack $ show tbl), ("prgm", String $ pack $ show tree), ("flops", Number $ fromIntegral flops)]
