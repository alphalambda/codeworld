{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}

module Extras.Do
       (($),(>>=),(>>),return,done,prompt,promptNumber,print
       ,today,now,weekDate
       ,numericValue,fromInt,fromDouble
       ,fail)
where

import qualified "base" Prelude as P
import           "base" Prelude ((>>=),(>>),return,Maybe(..),putStrLn,($))

import Internal.Text(fromCWText)

import Prelude hiding (Maybe(..))

import           GHCJS.DOM (currentWindow)
import qualified GHCJS.DOM.Window as W

import Internal.Text(fromCWText)
import qualified Data.Text.Read as R

import Data.Time.Clock (getCurrentTime)
import Data.Time.Calendar (toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.LocalTime (getCurrentTimeZone,utcToLocalTime,LocalTime(..),TimeOfDay(..))

-- | Terminates a do-block
done :: IO ()
done = return()

-- | Shows a dialog box with the given Text and lets the user to enter a Text answer
prompt :: Text -> IO Text
prompt(msg) = do
       Just window <- currentWindow
       name <- W.prompt window (Just (toString msg)) (Just (toString ""))
       case name of
           Just n -> return (fromString n)
           Nothing -> return ""

-- | Shows a dialog box with the given Text and lets the user to enter a Number answer
promptNumber :: Text -> IO Number
promptNumber(msg) = do
  n <- prompt(msg)
  return (numericValue(n))

-- | Outputs the given Text to the console
print :: Text -> IO ()
print(txt) = putStrLn (toString txt)

-- | Interprets the given Text as a Number if possible. Otherwise, the result is 0
numericValue :: Text -> Number
numericValue numeral =
    case R.double (fromCWText numeral) of
            P.Left _ -> 0
            P.Right (value,_) -> fromDouble value

-- | Gets the current year, month and day
today :: IO (Number,Number,Number)
today = do
        now' <- getCurrentTime
        timezone <- getCurrentTimeZone
        let zoneNow = utcToLocalTime timezone now'
        let (year, month, day) = toGregorian $ localDay zoneNow
        return (fromInteger year, fromInt month, fromInt day)

-- | Get the week of the year (1-53) and the current day of the week, where 1 is Monday and 7 is Sunday
weekDate :: IO (Number,Number)
weekDate = do
      now' <- getCurrentTime
      timezone <- getCurrentTimeZone
      let zoneNow = utcToLocalTime timezone now'
      let (_,weeknum,dow) = toWeekDate $ localDay zoneNow
      return (fromInt weeknum, fromInt dow)

-- | Gets the current hour, minute and second
now :: IO (Number,Number,Number)
now = do
      now' <- getCurrentTime
      timezone <- getCurrentTimeZone
      let zoneNow = utcToLocalTime timezone now'
      let timeNow = localTimeOfDay zoneNow
      return (fromInt $ todHour timeNow, fromInt $ todMin timeNow, fromInt $ P.truncate (todSec timeNow))

