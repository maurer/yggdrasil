module Handler.Distill where

import Import

import Database.Persist.Sql
import Language.Haskell.TH ( Exp(..) )

getDistillR :: Handler Html
getDistillR = do
  jotEnts  <- runDB $ selectList [JotCompleted ==. Nothing
                                 ,JotShelved   ==. Nothing]
                                 [Asc JotCreated]
  let jotButtons = map (\e -> (fromSqlKey $ entityKey e,
                               jotBody $ entityVal $ e)) jotEnts
  defaultLayout $ do
    setTitle "Distill."
    $(widgetFile "distill")
