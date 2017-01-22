{-# LANGUAGE FlexibleContexts, TypeOperators #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Data.Value.Utils
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved, Patrick Bahr, and Morten Ib Nielsen
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module contains utility functions for the AST defined in
-- 'Poets.Data.Value'.
--
--------------------------------------------------------------------------------
module Poets.Data.Value.Utils
    (
     extractRecord,
     extractField,
     fieldsMap',
     newFields',
     addField,
     addField',
     updateField,
     updateField',
     setField,
     setField',
     createDate,
     dateToYearMonthDay,
     createTime,
     timeToHourMinSecMicroSec,
     createDateTime,
     createDateTime',
     dateTimeToDateTime,
     dateTimeToMicroSeconds,
     dateTimeToComponents,
     microSecondsToDateTime,
     addDurationToDateTime,
     subtractDurationFromDateTime,
     dateTimeDiff,
     formatDateTime,
     getCurrentDateTime
    ) where

import Poets.Data.Value
import Control.Monad.Error
import Data.Comp
import Prelude hiding (and)
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Fixed (div', mod')
import qualified Data.DateTime as DT
import Data.Time.Calendar (toGregorian, fromGregorianValid)
import Data.Time.LocalTime (TimeOfDay(..), LocalTime(..), makeTimeOfDayValid, utcToLocalTime, localTimeToUTC, utc)

compareVFieldNames :: VField e1 -> VField e2 -> Ordering
compareVFieldNames f1 f2 = compare (vfieldName f1) (vfieldName f2)

fieldsMap' :: VFields e -> Map FieldName e
fieldsMap' = Map.map vfieldValue . fieldsMap



{-| This function constructs a new collection of fields from the given
list of fields similarly to 'newFields'. It is however executed in a
monadic contexts and will fail if the given list contains two fields
with the same name. -}

newFields' :: [VField e] -> Either String (VFields e)
newFields' fs = findDup mList >>
                return (newFields fs)
    where mList = map (\f-> (vfieldName f, f)) $ sortBy compareVFieldNames fs
          findDup ((x,_):xs@((y,_):_))
              | x == y = Left $ "field '"++ x ++ "' is defined twice"
              | otherwise = findDup xs
          findDup _ = return ()


-- |Extract a record from a general term.
extractRecord :: (Val :<: f) => Term f -> Either String (VRecord (Term f))
extractRecord e = case project e of
                    Just v -> case v of
                                VRecord r -> return r
                                _ -> Left "Value of record type expected"
                    Nothing -> Left "Value expected"

-- |Extract a record field from a general term.
extractField :: (Val :<: f) => FieldName -> Term f -> Either String (Term f)
extractField fn e = extractRecord e >>= lookupField fn

-- |Add a new field to a record value. Fails if the field already exists.
addField :: FieldName -> e -> VRecord e -> Either String (VRecord e)
addField fname val rec@VR{vrecordFields = fields} = do
  res <- addFieldValue fname val fields
  return rec{vrecordFields = res}

-- |Add a new field to a general term. Fails if the field already exists.
addField' :: (Val :<: f) => FieldName -> Term f -> Term f -> Either String (Term f)
addField' fn v e = do
  r <- extractRecord e
  r' <- addField fn v r
  return $ inject $ VRecord r'

-- |Update an existing field of a record value. Fails if the field does not
-- already exist.
updateField :: FieldName -> e -> VRecord e -> Either String (VRecord e)
updateField fname val rec@VR{vrecordFields = fields} = do
  res <- updateFieldValue fname val fields
  return rec{vrecordFields = res}

-- |Update an existing field of a general term. Fails if the field does not
-- already exist.
updateField' :: (Val :<: f) => FieldName -> Term f -> Term f -> Either String (Term f)
updateField' fn v e = do
  r <- extractRecord e
  r' <- updateField fn v r
  return $ inject $ VRecord r'

-- |Set the field of a record value. If the field already exists it is updated,
-- otherwise it is added.
setField :: FieldName -> e -> VRecord e -> VRecord e
setField fname val rec@VR{vrecordFields = fields} =
  rec{vrecordFields = setFieldValue fname val fields}

-- |Set the field of a general term. If the field already exists it is updated,
-- otherwise it is added.
setField' :: (Val :<: f) => FieldName -> Term f -> Term f -> Either String (Term f)
setField' fn v e = do
  r <- extractRecord e
  return $ inject $ VRecord $ setField fn v r

--------------------------------------------------------------------------------
-- Date, time, duration functions
--------------------------------------------------------------------------------

-- |Create a 'Date' value from year, month, and day.
createDate :: Integer -> Int -> Int -> Maybe Date
createDate = fromGregorianValid


-- |Extract the components of a 'Date' value.
dateToYearMonthDay :: Date -> (Integer, Int, Int)
dateToYearMonthDay = toGregorian

-- |Create a 'Time' value from hour, minute, second, and microsecond.
createTime :: Int -> Int -> Int -> Int -> Maybe Time
createTime hour min sec msec = do
  when (msec < 0 || msec >= 1000000) Nothing
  makeTimeOfDayValid hour min sec'
    where sec' | msec == 0 = fromIntegral sec
               | otherwise = fromRational $ fromIntegral sec +
                             fromIntegral msec / 1000000

-- |Extract the components of a 'Time' value.
timeToHourMinSecMicroSec :: Time -> (Int, Int, Int, Int)
timeToHourMinSecMicroSec t =
    let p = todSec t in
    (todHour t, todMin t, p `div'` 1, ((p * 1000000) `mod'` 1000000) `div'` 1)

-- |Create a 'DateTime' value from a 'Date' and a 'Time'.
createDateTime :: Date -> Time -> DateTime
createDateTime = LocalTime

-- |Create a 'DateTime' value from year, month, day, hour, minute,
-- second and microsecond.
createDateTime' :: Integer -> Int -> Int -> Int -> Int -> Int -> Int -> Maybe DateTime
createDateTime' y m d h min s ms = liftM2 LocalTime (createDate y m d) (createTime h min s ms)

-- |Extract the components of a 'DateTime' value.
dateTimeToDateTime :: DateTime -> (Date, Time)
dateTimeToDateTime dt = (localDay dt, localTimeOfDay dt)

dateTimeToComponents :: DateTime -> (Integer, Int, Int, Int, Int, Int, Int)
dateTimeToComponents dt = (y,m,d,h,min,s,ms)
  where (date,time) = dateTimeToDateTime dt
        (y,m,d) = dateToYearMonthDay date
        (h,min,s,ms) = timeToHourMinSecMicroSec time

-- |Convert a 'DateTime' value to microseconds since 1970.
dateTimeToMicroSeconds :: DateTime -> Integer
dateTimeToMicroSeconds dt =
    let (_,_,_,msec) = timeToHourMinSecMicroSec $ snd $ dateTimeToDateTime dt in
    1000000 * DT.toSeconds (localTimeToUTC utc dt) + fromIntegral msec

-- |Create a 'DateTime' value from microseconds since 1970.
microSecondsToDateTime :: Integer -> DateTime
microSecondsToDateTime n =
    let (sec,msec) = n `divMod` 1000000 in
    let t = utcToLocalTime utc (DT.fromSeconds sec) in
    let l = localTimeOfDay t in
    t{localTimeOfDay = l{todSec = todSec l + fromRational (fromIntegral msec / 1000000)}}

-- |Add a duration value to a DateTime value.
addDurationToDateTime :: VDuration Int -> DateTime -> DateTime
addDurationToDateTime d dt =
    let msec = dateTimeToMicroSeconds dt in
    let sec = toInteger (toSeconds d) in
    microSecondsToDateTime (msec + 1000000 * sec)

-- |Subtract a duration value from a DateTime value.
subtractDurationFromDateTime :: DateTime -> VDuration Int -> DateTime
subtractDurationFromDateTime t d = negateDuration d `addDurationToDateTime` t

-- |Compute the duration between two dates.
dateTimeDiff :: DateTime -> DateTime -> VDuration Int
dateTimeDiff start end =
    fromSeconds $ fromInteger $
                  localTimeToUTC utc start `DT.diffSeconds` localTimeToUTC utc end

formatDateTime :: String -> DateTime -> String
formatDateTime f dt = DT.formatDateTime f (localTimeToUTC utc dt)

getCurrentDateTime :: IO DateTime
getCurrentDateTime = liftM (utcToLocalTime utc) DT.getCurrentTime