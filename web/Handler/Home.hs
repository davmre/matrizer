module Handler.Home where

import Import
import Matrizer.Util

getHomeR :: Handler Html
getHomeR = defaultLayout (setTitle "Matrizer" >> $(widgetFile "main"))

postHomeR :: Handler Html
postHomeR = do
              postedText <- runInputPost $ ireq textField "program"
              defaultLayout (setTitle "Optimized" >> [whamlet|
<table border=0>
  <tr>
    <td valign=top>
      <p>Matrizer input:
      <form method=post action=@{HomeR}>
        <textarea cols=40 rows=20  name=program>#{postedText}
        <input type=submit value="Optimize">
    <td width=10% cellpadding=20>
    <td valign=top>
      <p>Matrizer output: 
      <pre>#{optimizeRawText postedText}
<p>
<p>
|])

optimizeRawText :: Text -> Text
optimizeRawText t = pack $ optimizeStr $ unpack t