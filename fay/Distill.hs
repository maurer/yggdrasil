{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}
module Distill where

import Prelude
import Fay.FFI
import Language.Fay.Yesod hiding (Text, fromString)
import Fay.Text
import SharedTypes
import DOM

setInnerHTML :: Element -> Text -> Fay ()
setInnerHTML = ffi "%1.innerHTML=%2"

onClick :: Element -> Fay () -> Fay ()
onClick = ffi "%1.onclick=%2"

jsLog :: Text -> Fay ()
jsLog = ffi "console.log(%1)"

main :: Fay ()
main = do
    jotViewer   <- getElementById "jotViewer"
    buttonGroup <- getElementById "jotButtons"
    buttonList  <- children buttonGroup
    let nl = nodeListToArray buttonList
    mapM_ (\b -> do
        onClick b $ do bVal <- getValue b
                       setInnerHTML jotViewer bVal 
      ) $ nodeListToArray buttonList
