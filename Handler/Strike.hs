module Handler.Strike where

import Import
import Database.Persist.Sql
import Data.Text (pack)
import Data.Time
import Control.Monad

taskPanel task = $(widgetFile "taskPanel")
journalPanel (Entity _ entry) = $(widgetFile "journalPanel")

execChildren (Entity tid task) = do
  if taskExec task
    then do
      rels <- selectList [TaskDependencyParent ==. tid] []
      let childIds = map (taskDependencyChild . entityVal) rels
      n <- count [TaskId <-. childIds, TaskCompleted ==. Nothing]
      return $ n == 0
    else return True
getStrikeR :: Handler Html
getStrikeR = do
  t <- liftIO getCurrentTime
  tasks <- runDB $ do
    rawTasks <- selectList
      ((TaskCompleted ==. Nothing) :
       ([TaskDelay ==. Nothing] ||. [TaskDelay <=. Just t]))
      [Asc TaskCreated]
    filterM execChildren rawTasks
  defaultLayout $ do
    setTitle "Strike."
    $(widgetFile "strike")

workWidget task taskId entries =
  if taskExec task
    then $(widgetFile "strikeExec")
    else $(widgetFile "strikeDefault")

getStrikeTaskR :: TaskId -> Handler Html
getStrikeTaskR taskId = do
  task <- runDB $ get404 taskId
  entries <- runDB $ selectList [TaskJournalParent ==. taskId]
                                [Desc TaskJournalTime]
  defaultLayout $ do
    setTitle "Strike."
    $(widgetFile "strikeTask")
