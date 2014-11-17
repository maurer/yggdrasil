module Handler.Jot where

import Import
import Data.Time
import Yesod.Form.Bootstrap3

jotForm :: Form Text
jotForm = renderBootstrap3 BootstrapBasicForm $
  areq textField
       (withAutofocus $ withLargeInput $ withPlaceholder "Idea." "")
       Nothing

getJotR :: Handler Html
getJotR = do
  (jotWidget, jotEnctype) <- generateFormPost jotForm
  defaultLayout $ do
    setTitle "Jot."
    $(widgetFile "jot")

getReadJotR :: JotId -> Handler TypedContent
getReadJotR jotId = do
  jot <- runDB $ get404 $ jotId
  selectRep $ do
    provideRep $ return $ object
      [ "body"      .= jotBody jot
      , "created"   .= jotCreated jot
      , "completed" .= jotCompleted jot
      ]

postCompleteJotR :: JotId -> Handler ()
postCompleteJotR jotId = do
  t <- liftIO getCurrentTime
  runDB $ update jotId [JotCompleted =. Just t]

postJotShelveR :: JotId -> Handler ()
postJotShelveR jotId = do
  t <- liftIO getCurrentTime
  runDB $ update jotId [JotShelved =. Just t]

postJotR :: Handler Html
postJotR = do
  ((result, _), _) <- runFormPost jotForm
  case result of
    FormSuccess r -> runDB $ do
      t <- liftIO getCurrentTime
      insert_ $ Jot
        { jotCreated   = t
        , jotBody      = r
        , jotCompleted = Nothing
        , jotShelved   = Nothing
        }
    _ -> error "dunkd"
  getJotR
