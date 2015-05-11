module Handler.OptimizeRaw where

import Import
import Matrizer

getOptimizeRawR :: Handler Html
getOptimizeRawR = defaultLayout $(widgetFile "main")

postOptimizeRawR :: Handler Html
postOptimizeRawR = do
                 postedText <- runInputPost $ ireq textField "program"
                 defaultLayout $ [whamlet|<pre>#{optimizeRawText postedText}|]

optimizeRawText :: Text -> Text
optimizeRawText t = pack $ optimizeStr $ unpack t