module Handler.Home where

import Import
import Matrizer.Util
import TestCases

data Example = Example
    { sname :: String
    , lname  :: String
    , txt :: String
    }


examples = [ Example "gaussianlik" "Gaussian log-density" $(readTest "gaussian.mtr")
           , Example "normaleqns" "Normal equations" $(readTest "normaleqns.mtr")
           , Example "wmslemma" "Matrix inversion lemma" $(readTest "matrix_inv_lemma1.mtr")
           , Example "deriv" "Calculus (super-alpha)" $(readTest "test_deriv.mtr")
            ]

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
         setTitle "Matrizer" 
         addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
         $(widgetFile "main")

