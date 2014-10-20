module Handler.Jot where

import Import
import Data.Time
import Yesod.Form.Bootstrap3

jotForm :: Form Text
jotForm = renderBootstrap3 BootstrapBasicForm $
  areq textField
       (withAutofocus $ withPlaceholder "Idea." "")
       Nothing
getJotR :: Handler Html
getJotR = do
  (jotWidget, jotEnctype) <- generateFormPost jotForm
  defaultLayout $ do
    setTitle "Jot."
    $(widgetFile "jot")

postJotR :: Handler Html
postJotR = do
  ((result, _), _) <- runFormPost jotForm
  case result of
    FormSuccess r -> runDB $ do
      t <- liftIO getCurrentTime
      insert_ $ Jot
        { jotCreated = t
        , jotBody = r
        , jotCompleted = Nothing
        }
    _ -> error "dunkd"
  getJotR
