module Handler.Strike where

import Import
import Database.Persist.Sql
import Data.Text (pack)

taskPanel task = $(widgetFile "taskPanel")

getStrikeR :: Handler Html
getStrikeR = do
  tasks <- runDB $ do
    liveTaskIds <- selectKeysList [TaskCompleted ==.Nothing] []
    schedIds <- fmap (map $ scheduleTask . entityVal) $
                     selectList [ScheduleBonus ==. False
                                ,ScheduleTask <-. liveTaskIds]
                                [Asc SchedulePrio]
    ids <-
      if null schedIds
        then fmap (map $ scheduleTask . entityVal) $
                  selectList [ScheduleBonus ==. True
                             ,ScheduleTask <-. liveTaskIds]
                             [Asc SchedulePrio]
        else return $ schedIds
    selectList [TaskId <-. ids] [Asc TaskCreated]
  defaultLayout $ do
    setTitle "Strike."
    $(widgetFile "strike")

getStrikeTaskR :: TaskId -> Handler Html
getStrikeTaskR taskId = do
  task <- runDB $ get404 taskId
  let taskName = taskTitle task
  defaultLayout $ do
    setTitle $ "Strike: " <> (toHtml $ taskName)
    $(widgetFile "strikeTask")
