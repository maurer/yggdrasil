module Handler.Task where

import Import
import Data.Time

postTaskR :: Handler TypedContent
postTaskR = do
  goal  <- runInputPost $ ireq textField "goal"
  title <- runInputPost $ ireq textField "title"
  t    <- liftIO getCurrentTime
  taskId <- runDB $ insert $ Task
    { taskTitle = title
    , taskCompletion = goal
    , taskCreated = t
    , taskCompleted = Nothing
    }
  selectRep $ do
    provideRep $ return $ object
      [ "taskId" .= taskId ]

postTaskCompleteR :: TaskId -> Handler ()
postTaskCompleteR taskId = do
  t <- liftIO getCurrentTime
  runDB $ update taskId [TaskCompleted =. Just t]
