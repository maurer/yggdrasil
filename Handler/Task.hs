module Handler.Task where

import Import
import Data.Time
import Database.Persist.Sql

postTaskR :: Handler TypedContent
postTaskR = do
  goal    <- runInputPost $ ireq textField "goal"
  title   <- runInputPost $ ireq textField "title"
  exec    <- runInputPost $ ireq boolField "exec"
  mParent <- fmap (fmap toSqlKey) $ runInputPost $ iopt intField "parent"
  t    <- liftIO getCurrentTime
  let parentAction childId = case mParent of
        Just parent -> insert $ TaskDependency
          { taskDependencyParent = parent
          , taskDependencyChild  = childId
          }
        Nothing -> return undefined
  taskId <- runDB $ do
    tid <- insert $ Task
      { taskTitle = title
      , taskCompletion = goal
      , taskCreated = t
      , taskCompleted = Nothing
      , taskExec = exec
      , taskDelay = Nothing
      }
    _ <- parentAction tid
    return tid
  selectRep $ do
    provideRep $ return $ object
      [ "taskId" .= taskId ]

postTaskCompleteR :: TaskId -> Handler ()
postTaskCompleteR taskId = do
  t <- liftIO getCurrentTime
  runDB $ update taskId [TaskCompleted =. Just t]

postTaskDelayR :: TaskId -> Handler ()
postTaskDelayR taskId = do
  t <- liftIO getCurrentTime
  delay <- runInputPost $ ireq intField "delay"
  let t' = t {utctDay = addDays delay (utctDay t)}
  runDB $ update taskId [TaskDelay =. Just t']
