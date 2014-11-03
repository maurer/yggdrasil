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
import Language.Fay.Time
#else
import Language.Fay.Yesod
import Data.Time
--import Language.Fay.FFI
#endif

data FayJot = FayJot
  { body :: Text
  , created   :: UTCTime
  , completed :: Maybe UTCTime
  } deriving (Typeable, Data, Show)

data Command = GetFib Int (Returns Int)
             | GetJot Int (Returns FayJot)
             | CompleteJot Int (Returns ())
    deriving (Read, Typeable, Data)

