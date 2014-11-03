{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE PackageImports    #-}
module Distill where

import Prelude
import Fay.FFI
import Language.Fay.Yesod hiding (Text, fromString)
import Fay.Text
import SharedTypes
import DOM
import Language.Fay.Time

setInnerHTML :: Element -> Text -> Fay ()
setInnerHTML = ffi "%1.innerHTML=%2"

onClick :: Element -> Fay () -> Fay ()
onClick = ffi "%1.onclick=%2"

jsLog :: Text -> Fay ()
jsLog = ffi "console.log(%1)"

main :: Fay ()
main = do
    jotBody     <- getElementById "jotBody"
    jotCreated  <- getElementById "jotCreated"
    buttonGroup <- getElementById "jotButtons"
    jotComplete <- getElementById "jotComplete"
    buttonList  <- children buttonGroup
    mapM_ (\b -> do
        onClick b $ do jotIdS <- getValue b
                       jotId  <- parseInt jotIdS
                       let reset = do
                             setInnerHTML jotBody ""
                             setInnerHTML jotCreated ""
                             setInnerHTML jotComplete ""
                       call (GetJot jotId) $ \jot -> do
                         setInnerHTML jotBody    $ body jot
                         localTime <- renderLocalTime $ created jot
                         setInnerHTML jotCreated  $ localTime
                         setInnerHTML jotComplete $ "<button class=btn>Complete!</button>"
                         onClick jotComplete $
                           call (CompleteJot jotId)
                                (\_ -> do reset
                                          removeChild buttonGroup b)
      ) $ nodeListToArray buttonList
