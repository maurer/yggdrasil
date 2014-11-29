module Handler.Strike where

import Import
import Database.Persist.Sql
import Data.Text (pack)

taskPanel task = $(widgetFile "taskPanel")

getStrikeR :: Handler Html
getStrikeR = undefined

getStrikeTaskR :: TaskId -> Handler Html
getStrikeTaskR taskId = undefined
