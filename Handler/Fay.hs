module Handler.Fay where

import Import
import Data.Time
import Yesod.Fay
import Fay.Convert (readFromFay)
import Database.Persist.Sql

onCommand :: CommandHandler App
onCommand render command =
    case readFromFay command of
      Just (GetJot jotId r) -> do
        jot <- runDB $ get404 $ toSqlKey $ fromIntegral jotId
        render r $ FayJot 
          {body      = jotBody jot
          ,created   = jotCreated jot
          ,completed = jotCompleted jot
          }
      Just (CompleteJot rawJotId r) -> do
        let jotId :: (Key Jot) = toSqlKey $ fromIntegral rawJotId
        t <- liftIO getCurrentTime
        runDB $ update jotId [JotCompleted =. Just t]
        render r ()
      Nothing               -> invalidArgs ["Invalid command"]
