module Handler.Strike where

import Import
import Database.Persist.Sql

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
