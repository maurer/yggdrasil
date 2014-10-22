{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyDataDecls    #-}
module Distill where

import Prelude
import Fay.FFI
import Language.Fay.Yesod
import SharedTypes

data Element

getElementById :: String -> Fay Element
getElementById = ffi "document.getElementById(%1)"

getElementsByTagName :: Element -> String -> Fay [Element]
getElementsByTagName = ffi "%1.getElementsByTagName(%2)"

getAttribute :: String -> Element -> Fay String
getAttribute = ffi "%2[%1]"

getProp :: String -> Element -> Fay String
getProp = ffi "%2.%1"

setInnerHTML :: Element -> String -> Fay ()
setInnerHTML = ffi "%1.innerHTML=%2"

onClick :: Element -> Fay () -> Fay ()
onClick = ffi "%1.onclick=%2"

onKeyUp :: Element -> Fay () -> Fay ()
onKeyUp = ffi "%1.onkeyup=%2"

alert :: String -> Fay ()
alert = ffi "window.alert(%1)"

parseInt :: String -> Fay Int
parseInt = ffi "window.parseInt(%1, 10)"

jsLog :: String -> Fay ()
jsLog = ffi "console.log(%1)"

main :: Fay ()
main = do
    alert "test"
    jsLog 3 3
    buttonGroup <- getElementById "jotButtons"
    buttons     <- getElementsByTagName buttonGroup "button"
    mapM_ (\b -> do
        jsLog "button bind"
        jsLog b
        onClick b $ alert $ getAttribute "value" b
      ) buttons
