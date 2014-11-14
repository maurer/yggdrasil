module Handler.Scheme where

import Import
import Database.Persist.Sql

taskPanel task = $(widgetFile "taskPanel")

getSchemeR :: Handler Html
getSchemeR = do
  [unsched, sched, bonus] <- runDB $ do
    liveTasks <- selectList [TaskCompleted ==. Nothing] [Asc TaskCreated]
    let liveTaskIds = map entityKey liveTasks
    schedIds <- fmap (map $ scheduleTask . entityVal) $ 
                     selectList [ScheduleBonus ==. False
                                ,ScheduleTask <-. liveTaskIds]
                                [Asc SchedulePrio]
    bonusIds <- fmap (map $ scheduleTask . entityVal) $ 
                     selectList [ScheduleBonus ==. True
                                ,ScheduleTask <-. liveTaskIds]
                                [Asc SchedulePrio]
    let unschedIds =
          filter (\x -> not ((elem x schedIds) || (elem x bonusIds)))
                 liveTaskIds
    return $ map (flip keyGet liveTasks) [unschedIds, schedIds, bonusIds]
  defaultLayout $ do
    setTitle "Scheme."
    $(widgetFile "scheme")
  where keyGet keys = filter (flip elem keys . entityKey) 
