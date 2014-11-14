module Handler.Schedule where

import Import
import Database.Persist.Sql

listField :: Monad m => RenderMessage (HandlerSite m) FormMessage
          => Field m a
          -> Field m [a]
listField field = Field listParse undefined UrlEncoded
  where listParse elems _ = do
          parses <- mapM (\x -> fieldParse field [x] []) elems
          return $ do inner <- sequence parses
                      return $ sequence inner

postScheduleR :: Handler ()
postScheduleR = do
  (bonus, schedNums) <- runInputPost $ (,)
    <$> ireq boolField "bonus"
    <*> ireq (listField intField) "sched[]"
  let sched = map toSqlKey schedNums
  runDB $ do deleteWhere [ScheduleBonus ==. bonus]
             mapM_ insert (zipWith3 Schedule [0..] (repeat bonus) sched)
