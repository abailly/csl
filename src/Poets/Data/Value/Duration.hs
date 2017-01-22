{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Data.Value.Duration
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved, Patrick Bahr, and Morten Ib Nielsen
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines duration values and operations on them.
--
--------------------------------------------------------------------------------

module Poets.Data.Value.Duration
    (
     VDuration(..),
     normaliseDuration,
     toSeconds,
     fromSeconds,
     scaleDuration,
     addDuration,
     negateDuration,
     subtractDuration,
     ) where

import Data.Foldable
import Data.Comp.Derive
import Data.Ord
import Control.Applicative
import Prelude hiding (and, concat)


{-| This data type represents time durations (or time differences).
-}

data VDuration a = VD {
      durationSeconds :: a,
      durationMinutes :: a,
      durationHours :: a,
      durationDays :: a,
      durationWeeks :: a,
      durationMonths :: a,
      durationYears :: a
    }

instance (Show a) => Show (VDuration a) where
    show d = concat $ zipWith (++) (map show (toList d)) ["seconds, ","minutes, ","hours, ","days, ","weeks, ", "months, ","years"]


-- |Convert a duration value to seconds. NB: Currently we assume one month is 30
-- days, and one year is 12 months.
toSeconds :: VDuration Int -> Int
toSeconds VD{durationSeconds = s,
             durationMinutes = m,
             durationHours = h,
             durationDays = d,
             durationWeeks = w,
             durationMonths = mo,
             durationYears = y} =
    s + 60 * (m + 60 * (h + 24 * (d + (7 * w) + 30 * (mo + 12 * y))))

-- |Create a duration value from a number of seconds.
fromSeconds :: Int -> VDuration Int
fromSeconds s = normaliseDuration VD{durationSeconds = s,
                                     durationMinutes = 0,
                                     durationHours = 0,
                                     durationDays = 0,
                                     durationWeeks = 0,
                                     durationMonths = 0,
                                     durationYears = 0}

{-|
  Normalise a duration value.
-}
normaliseDuration :: VDuration Int -> VDuration Int
normaliseDuration vd =
    let (years, r1) = toSeconds vd `divMod` (60 * 60 * 24 * 30 * 12) in
    let (months, r2) = r1 `divMod` (60 * 60 * 24 * 30) in
    let (weeks, r3) = r2 `divMod` (60 * 60 * 24 * 7) in
    let (days, r4) = r3 `divMod` (60 * 60 * 24) in
    let (hours, r5) = r4 `divMod` (60 * 60) in
    let (minutes, seconds) = r5 `divMod` 60 in
    VD{durationSeconds = seconds,
       durationMinutes = minutes,
       durationHours = hours,
       durationDays = days,
       durationWeeks = weeks,
       durationMonths = months,
       durationYears = years}

instance Eq (VDuration Int) where
    d == e = and $ fmap (==) d' <*> e'
        where d' = normaliseDuration d
              e' = normaliseDuration e

instance Ord (VDuration Int) where
    compare = comparing toSeconds

               
$( derive [makeFunctor, makeFoldable, makeTraversable] [''VDuration] )


instance Applicative VDuration where
    pure v = VD { durationSeconds = v,
                  durationMinutes = v,
                  durationHours = v,
                  durationDays = v,
                  durationWeeks = v,
                  durationMonths = v,
                  durationYears = v }
           
    VD { durationSeconds = fs,
         durationMinutes = fmin,
         durationHours = fh,
         durationDays = fd,
         durationWeeks = fw,
         durationMonths = fmon,
         durationYears = fy }
           <*>
           VD { durationSeconds = s,
                durationMinutes = min,
                durationHours = h,
                durationDays = d,
                durationWeeks = w,
                durationMonths = mon,
                durationYears = y }
               = VD { durationSeconds = fs s,
                      durationMinutes = fmin min,
                      durationHours = fh h,
                      durationDays = fd d,
                      durationWeeks = fw w,
                      durationMonths = fmon mon,
                      durationYears = fy y }

-- |Negate a duration value.
negateDuration :: (Num a) => VDuration a -> VDuration a
negateDuration = fmap negate

-- |Add two duration values.
addDuration :: (Num a) => VDuration a -> VDuration a -> VDuration a
addDuration d e = pure (+) <*> e <*> d

-- |Subtract two duration values.
subtractDuration :: (Num a) => VDuration a -> VDuration a -> VDuration a
subtractDuration d e = d `addDuration` negateDuration e

-- |Scale a duration value by the supplied factor.
scaleDuration :: (Num a) => a -> VDuration a -> VDuration a
scaleDuration n = fmap (* n)