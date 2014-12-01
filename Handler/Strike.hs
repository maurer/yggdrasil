module Handler.Strike where

import Import
import Database.Persist.Sql
import Data.Text (pack)
import Data.Time

taskPanel task = $(widgetFile "taskPanel")

getStrikeR :: Handler Html
getStrikeR = do
  t <- liftIO getCurrentTime
  tasks <- runDB $ selectList ((TaskCompleted ==. Nothing) :
                               ([TaskDelay ==. Nothing]
                                ||. [TaskDelay <=. Just t])
                              )
                              [Asc TaskCreated]
  defaultLayout $ do
    setTitle "Strike."
    $(widgetFile "strike")

workWidget task taskId =
  if taskExec task
    then $(widgetFile "strikeExec")
    else [whamlet|No tools.|]

getStrikeTaskR :: TaskId -> Handler Html
getStrikeTaskR taskId = do
  task <- runDB $ get404 taskId
  defaultLayout $ do
    setTitle "Strike."
    $(widgetFile "strikeTask")
