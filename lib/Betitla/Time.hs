{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes   #-}

module Betitla.Time
( PhaseOfDay (..)
, Duration (..)
, DurationRating (..)
, durationToRating
, pickDurationRatingTerm
, toSec
, toMin
, toHour
, localTimeToPOD
, pickPhaseOfDayTerm
) where

import           GHC.Generics

import           Betitla.Sport
import           Betitla.Term
import           Betitla.Util

import           Data.Aeson                (FromJSON)
import           Data.Time.LocalTime       (LocalTime (..), TimeOfDay (..))
import           Path                      (Abs, File, absfile)

import           Test.QuickCheck           (Gen, oneof)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary, shrink)

data PhaseOfDay = PreDawn
                | Morning
                | Afternoon
                | Evening
                | LateNight
                deriving (Show, Eq, Generic)

instance Term PhaseOfDay
instance FromJSON PhaseOfDay

instance Arbitrary PhaseOfDay where
  arbitrary = oneof $ pure <$> [PreDawn, Morning, Afternoon, Evening, LateNight]

localTimeToPOD :: LocalTime -> PhaseOfDay
localTimeToPOD (LocalTime _ (TimeOfDay hour _ _))
  | hour < 3  = LateNight
  | hour < 6  = PreDawn
  | hour < 12 = Morning
  | hour < 18 = Afternoon
  | hour < 9  = Evening
  | otherwise = LateNight

-- | Pick a DistanceRating from the DistanceRating.terms file which is read at runtime.
pickPhaseOfDayTerm :: PhaseOfDay -> IO String
pickPhaseOfDayTerm = pickRatingTerm [absfile|/Users/jmagee/src/betitla.git/PhaseOfDay.terms|]

data Duration = Seconds Int
              | Minutes Int
              | Hours Int
              deriving (Show)

toSec :: Duration -> Duration
toSec (Minutes m) = Seconds $ m * 60
toSec h@(Hours _) = (toSec . toMin) h
toSec x           = x

toMin :: Duration -> Duration
toMin (Seconds s) = Minutes $ s `div` 60
toMin (Hours h)   = Minutes $ h * 60
toMin m           = m

toHour :: Duration -> Duration
toHour s@(Seconds _) = (toHour . toMin) s
toHour (Minutes m)  = Hours $ m `div` 60
toHour h            = h

instance Eq Duration where
  (Seconds s1)  == (Seconds s2)  = s1 == s2
  x             == y             = toSec x == toSec y

instance Ord Duration where
  compare (Seconds s1) (Seconds s2) = compare s1 s2
  compare x            y            = compare (toSec x) (toSec y)

instance Arbitrary Duration where
  arbitrary = oneof [Seconds <$> (arbitrary :: Gen Int), Minutes <$> (arbitrary :: Gen Int),
                     Hours <$> (arbitrary :: Gen Int)]
  shrink x@(Hours __) = [toMin x]
  shrink x@(Minutes __) = [toSec x]
  shrink _ = []

data DurationRating = VeryShort
                    | Short
                    | Average
                    | Long
                    | VeryLong
                    | InsaneLong
                    deriving (Show, Eq, Generic)

instance Term DurationRating
instance FromJSON DurationRating
instance Arbitrary DurationRating where
  arbitrary = oneof $ pure <$> durationRatings

durationRatings :: [DurationRating]
durationRatings = [VeryShort, Short, Average, Long, VeryLong, InsaneLong]

-- | Convert a Duration into a DurationRating.
durationToRating :: Sport -> Duration -> DurationRating
durationToRating sport dur = select dur (pickDtable sport) durationRatings
  where
    pickDtable Ride = [Minutes 30, Hours 1, Hours 2, Hours 6, Hours 10]
    pickDtable Run  = [Minutes 15, Minutes 30, Hours 1, Hours 2, Hours 4]
    pickDtable Walk = [Minutes 10, Minutes 30, Hours 1, Hours 2, Hours 3]
    pickDtable Hike = [Minutes 30, Hours 1, Hours 2, Hours 8, Hours 10]
    pickDtable AlpineSki = [Minutes 30, Hours 1, Hours 2, Hours 4, Hours 8]
    pickDtable Golf = [Minutes 15, Minutes 30, Hours 1, Hours 2, Hours 3]

-- | Pick a DistanceRating from the DistanceRating.terms file which is read at runtime.
pickDurationRatingTerm :: DurationRating -> IO String
pickDurationRatingTerm = pickRatingTerm [absfile|/Users/jmagee/src/betitla.git/DurationRating.terms|]
