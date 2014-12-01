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
      , taskEvidence = Nothing
      }
    _ <- parentAction tid
    return tid
  selectRep $ do
    provideRep $ return $ object
      [ "taskId" .= taskId ]

postTaskCompleteR :: TaskId -> Handler ()
postTaskCompleteR taskId = do
  t <- liftIO getCurrentTime
  ev <- runInputPost $ iopt textField "evidence"
  let evUpdate = case ev of
        Just evidence -> [TaskEvidence =. Just evidence]
        Nothing -> []
  runDB $ update taskId $ (TaskCompleted =. Just t) : evUpdate

postTaskDelayR :: TaskId -> Handler ()
postTaskDelayR taskId = do
  t <- liftIO getCurrentTime
  delay <- runInputPost $ ireq intField "delay"
  let t' = t {utctDay = addDays delay (utctDay t)}
  runDB $ update taskId [TaskDelay =. Just t']

postTaskJournalR :: TaskId -> Handler ()
postTaskJournalR taskId = do
  t <- liftIO getCurrentTime
  entry <- runInputPost $ ireq textField "entry"
  runDB $ insert_ $ TaskJournal taskId t entry
