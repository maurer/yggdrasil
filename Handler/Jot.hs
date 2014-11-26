module Handler.Jot where

import Import
import Data.Time
import Yesod.Form.Bootstrap3

jotForm :: Form Text
jotForm = renderBootstrap3 BootstrapBasicForm $
  areq textField
       (withAutofocus $ withLargeInput $ withPlaceholder "Idea." "")
       Nothing

getNewJotR :: Handler Html
getNewJotR = do
  (jotWidget, jotEnctype) <- generateFormPost jotForm
  defaultLayout $ do
    setTitle "Jot."
    $(widgetFile "jot")

getJotR :: JotId -> Handler TypedContent
getJotR jotId = do
  jot <- runDB $ get404 $ jotId
  selectRep $ do
    provideRep $ return $ object
      [ "body"      .= jotBody jot
      , "created"   .= jotCreated jot
      , "completed" .= jotCompleted jot
      ]
deleteJotR :: JotId -> Handler ()
deleteJotR jotId = do
  t <- liftIO getCurrentTime
  runDB $ update jotId [JotDeleted =. Just t]

postCompleteJotR :: JotId -> Handler ()
postCompleteJotR jotId = do
  t <- liftIO getCurrentTime
  runDB $ update jotId [JotCompleted =. Just t]

postNewJotR :: Handler Html
postNewJotR = do
  ((result, _), _) <- runFormPost jotForm
  case result of
    FormSuccess r -> runDB $ do
      t <- liftIO getCurrentTime
      insert_ $ Jot
        { jotCreated   = t
        , jotBody      = r
        , jotCompleted = Nothing
        , jotDeleted   = Nothing
        }
    _ -> error "dunkd"
  getNewJotR
