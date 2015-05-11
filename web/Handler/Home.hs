module Handler.Home where

import Import
import Matrizer.Util

getHomeR :: Handler Html
getHomeR = defaultLayout (setTitle "Matrizer" >> $(widgetFile "main"))

postHomeR :: Handler Html
postHomeR = do
              postedText <- runInputPost $ ireq textField "program"
              defaultLayout (setTitle "Optimized" >> [whamlet|
<p>Matrizer output: 
<pre>#{optimizeRawText postedText}
<p>
  <a href=@{HomeR}>Try again!
<p>
|])

optimizeRawText :: Text -> Text
optimizeRawText t = pack $ optimizeStr $ unpack t