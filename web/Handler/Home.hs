module Handler.Home where

import Import
import Matrizer.Util

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
         setTitle "Matrizer" 
         addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
         $(widgetFile "main")

