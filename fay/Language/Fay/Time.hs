-- This file is copyright Chris Done, and is available under BSD3
-- It is imported directly here to deal with yesod-fay not yet supporting new
-- fay-base, and will be removed when it does
{-# OPTIONS -fno-warn-missing-methods #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}

-- | A limited subset of the time package.

module Language.Fay.Time
  (-- * Compatible with the time package
  getCurrentTime
  ,fromGregorian
  ,UTCTime
  ,Day
  ,utctDay
  -- * Incompatible Fay-specific helpers
  ,showTime
  ,showDay
  ,renderLocalTime
  )
  where

import Fay.FFI
import Language.Fay.Yesod hiding (Text, fromString)
import Fay.Text
import Data.Data

-- | Date representation (internally represented as milliseconds from Epoch).
data UTCTime
    deriving (Typeable)

-- We provide no methods, this is just to satisfy type-safety. No
-- methods work in Fay anyway.
instance Data UTCTime
instance Show UTCTime
instance Eq UTCTime
instance Ord UTCTime

-- | Day representation (internally represented as milliseconds from Epoch).
data Day
    deriving (Typeable)
-- We provide no methods, this is just to satisfy type-safety. No
-- methods work in Fay anyway.
instance Data Day
instance Show Day
instance Eq Day
instance Ord Day

-- | Get the current time.
getCurrentTime :: Fay UTCTime
getCurrentTime = ffi "(new Date()).getTime()"

-- | Convert from proleptic Gregorian calendar. First argument is
-- year, second month number (1-12), third day (1-31).
fromGregorian :: Int -- ^ Year.
              -> Int -- ^ Month.
              -> Int -- ^ Day.
              -> Day
fromGregorian = ffi "Date.UTC(%1,%2-1,%3)"

-- | Extract the day from the time.
utctDay :: UTCTime -> Day
utctDay = ffi "%1"

-- | Show a time. Meant for debugging purposes, not production presentation.
showTime :: UTCTime -> Text
showTime = ffi "new Date(%1).toString()"

renderLocalTime :: UTCTime -> Fay Text
renderLocalTime = ffi "(new Date(%1).toLocaleString())"

-- | Show a day. Meant for debugging purposes, not production presentation.
showDay :: Day -> Text
showDay =
  ffi "date.getUTCFullYear() + ' ' + showMonth(date) + ' ' + (date.getUTCDate() + 1)"
