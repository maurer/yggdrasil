module Handler.Strike where

import Import
import Database.Persist.Sql
import Data.Text (pack)

taskPanel task = $(widgetFile "taskPanel")

getStrikeR :: Handler Html
getStrikeR = do
  tasks <- runDB $ selectList [TaskCompleted ==. Nothing] [Asc TaskCreated]
  defaultLayout $ do
    setTitle "Strike."
    $(widgetFile "strike")

getStrikeTaskR :: TaskId -> Handler Html
getStrikeTaskR taskId = do
  task <- runDB $ get404 taskId
  defaultLayout $ do
    setTitle "Strike."
    $(widgetFile "strikeTask")
