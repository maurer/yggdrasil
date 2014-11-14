module Handler.Scheme where

import Import

getSchemeR :: Handler Html
getSchemeR = do
  defaultLayout $ do
    setTitle "Scheme."
    $(widgetFile "scheme")
