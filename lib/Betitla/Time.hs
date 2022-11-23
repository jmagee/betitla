{-# LANGUAGE DeriveGeneric #-}

module Betitla.Time
( PhaseOfDay (..)
, Duration (..)
, DurationRating (..)
--, Holiday (..)
, CalDay (..)
, durationToRating
, toSec
, toMin
, toHour
, localTimeToPOD
, textToTimeZone
, utcToCal
, localTimeToCal
, (!==!)
) where

import           Betitla.Sport
import           Betitla.Term
import           Betitla.Util

import           Data.Aeson                (FromJSON)
import           Data.Either               (fromRight)
import           Data.Time.Calendar        (toGregorian)
import           Data.Time.Clock           (UTCTime, utctDay)
import           Data.Time.LocalTime       (LocalTime (..), TimeOfDay (..),
                                            TimeZone (..))
import           GHC.Generics              (Generic)
import           Text.Parsec               (anyChar, char, choice, count, digit,
                                            many, parse, spaces, string)

import           Test.QuickCheck           (Gen, oneof)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary, shrink)

data PhaseOfDay = PreDawn
                | Morning
                | Afternoon
                | Evening
                | LateNight
                deriving (Show, Eq, Generic)

instance Term PhaseOfDay where
  termFile = const "PhaseOfDay.terms"
instance FromJSON PhaseOfDay

instance Arbitrary PhaseOfDay where
  arbitrary = oneof $ pure <$> [PreDawn, Morning, Afternoon, Evening, LateNight]

{-data Holiday = Halloween-}
             {-| Christmas-}
             {-| GroundHogsDay-}
             {-| ValentinesDay-}
             {-| NewYearsEve-}
             {-| NewYearsDay-}
             {-| CincoDeMayo-}
             {-| Juneteenth-}
             {-| BastilleDay-}
             {-| MemorialDay-}
             {-| LaborDay-}
             {-| FourthOfJuly-}
             {-| ThanksgivingUS-}
             {-| ThanksgivingCanada-}
             {-deriving (Show, Eq, Generic)-}

{-instance FromJSON Holiday-}
-- ^ Actually we may need  this.  The reason is, for having different terms for the holidays.
-- or maybe not, why not just put them in the Hols?

data CalDay =
  CalDay { _year  :: Integer
         , _month :: Int
         , _day   :: Int
         } deriving (Show, Eq, Generic)

instance FromJSON CalDay

(!==!) :: CalDay -> CalDay -> (Bool, Bool, Bool)
(CalDay a b c) !==! (CalDay x y z) = (a == x, b == y, c == z)

-- | Create a CalDay from the triple of Year, Month, and Day.
calDayFromTriple :: (Integer, Int, Int) -> CalDay
calDayFromTriple (a, b, c) = CalDay a b c

localTimeToPOD :: LocalTime -> PhaseOfDay
localTimeToPOD (LocalTime _ (TimeOfDay hour _ _))
  | hour < 3  = LateNight
  | hour < 6  = PreDawn
  | hour < 12 = Morning
  | hour < 18 = Afternoon
  | hour < 9  = Evening
  | otherwise = LateNight

-- | "get-the-job-done" function to convert a "GMT+-XX:XXX" string into a timezone.
-- E.g. "(GMT-08:00) America/Los_Angeles"
textToTimeZone :: String -> TimeZone
textToTimeZone s = fromRight (TimeZone 0 False "") $ parse utcInfo s s
  where
    utcInfo = do
      _ <- char '('
      _ <- string "GMT"
      p <- choice [char '-', char '+']
      h <- count 2 digit
      _ <- char ':'
      m <- count 2 digit
      _ <- char ')'
      _ <- spaces
      rest <- many anyChar
      let hours   = read h :: Int
          minutes = read m :: Int
          sign    = if p == '+' then 1 else (-1)
      pure $ TimeZone (sign * hours * 60 + minutes) False rest

-- | Pick a DistanceRating from the DistanceRating.terms file which is read at runtime.
{-pickPhaseOfDayTerm :: PhaseOfDay -> IO String-}
{-pickPhaseOfDayTerm = pickRatingTerm [absfile|/Users/jmagee/src/betitla.git/PhaseOfDay.terms|]-}

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

instance Term DurationRating where
  termFile = const "DurationRating.terms"
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
    pickDtable _ = undefined -- FIXME

-- | Convert a UTC time to CalDay.
utcToCal :: UTCTime -> CalDay
utcToCal = calDayFromTriple . toGregorian . utctDay

-- | Convert a local time to CalDay.
localTimeToCal :: LocalTime -> CalDay
localTimeToCal = calDayFromTriple . toGregorian . localDay
