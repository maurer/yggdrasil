module Handler.Fay where

import Import
import Yesod.Fay
import Fay.Convert (readFromFay)
import Database.Persist.Sql

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

onCommand :: CommandHandler App
onCommand render command =
    case readFromFay command of
      Just (GetFib index r) -> render r $ fibs !! index
      Just (GetJot jotId r) -> do
        jot <- runDB $ get404 $ toSqlKey $ fromIntegral jotId
        render r $ FayJot 
          {body      = jotBody jot
          ,created   = jotCreated jot
          ,completed = jotCompleted jot
          }
        
      Nothing               -> invalidArgs ["Invalid command"]
