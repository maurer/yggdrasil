{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PackageImports #-}
module SharedTypes where

import Prelude
import Data.Data
#ifdef FAY
import Language.Fay.Yesod hiding (Text, fromString)
import Fay.Text
import FFI
#else
import Language.Fay.Yesod
--import Language.Fay.FFI
#endif

data FayJot = FayJot
  { body :: Text
  , created   :: Text
  , completed :: Maybe Text
  } deriving (Typeable, Data, Show)

data Command = GetFib Int (Returns Int)
             | GetJot Int (Returns FayJot)
    deriving (Read, Typeable, Data)

