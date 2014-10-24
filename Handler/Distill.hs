module Handler.Distill where

import Import

import Database.Persist.Sql
import Language.Haskell.TH ( Exp(..) )
import qualified Data.Text as T

elipsize :: Int -> Text -> Text
elipsize n s | T.length s <= n = s
             | otherwise = T.snoc (T.take (n - 1) s) '…'

getDistillR :: Handler Html
getDistillR = do
  jotEnts  <- runDB $ selectList [JotCompleted ==. Nothing] [Asc JotCreated]
  let jotButtons = map (\e -> (fromSqlKey $ entityKey e,
                               elipsize 25 $ jotBody $ entityVal $ e)) jotEnts
  defaultLayout $ do
    $(widgetFile "distill")
    $(fayFile' (ConE 'StaticR) "Distill")
