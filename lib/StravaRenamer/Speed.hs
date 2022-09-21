module StravaRenamer.Speed
( Speed (..)
, SpeedRating (..)
, speedToRating
, toMPS
, toKPH
, toMPK
) where

import           StravaRenamer.Sport
import           StravaRenamer.Util

import           Test.QuickCheck           (Gen, oneof)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary, shrink)

data Speed = MetersPerSec Float
           | KmPerHour Float
           | MinPerKm Float
           deriving (Show)

-- | Convert to meters-per-second.
toMPS :: Speed -> Speed
toMPS (KmPerHour kph) = MetersPerSec $ kph / 3.6
toMPS mps@(MinPerKm _) = (toMPS . toKPH) mps
toMPS x = x

toKPH :: Speed -> Speed
toKPH (MetersPerSec mps) = KmPerHour $ mps * 3.6
toKPH (MinPerKm mpk) = KmPerHour $ (1.0 / mpk) * 60.0
toKPH x = x

-- formula kph to mpk
-- 1 / (kph / 60)
toMPK :: Speed -> Speed
toMPK (KmPerHour kph) = MinPerKm $ 1 / (kph / 60)
toMPK mps@(MetersPerSec _) = (toMPK . toKPH) mps
toMPK x = x

instance Eq Speed where
  (KmPerHour a) == (KmPerHour b) = a == b
  a             == b             = (toKPH a) == (toKPH b)

instance Ord Speed where
  compare (KmPerHour a) (KmPerHour b) = compare a b
  compare a             b             = compare (toKPH a) (toKPH b)

instance Arbitrary Speed where
  arbitrary = oneof [MetersPerSec <$> (arbitrary :: Gen Float),
                     KmPerHour <$> (arbitrary :: Gen Float),
                     MinPerKm <$> (arbitrary :: Gen Float)]
  shrink x@(MinPerKm _) = [toKPH x]
  shrink x@(KmPerHour _) = [toMPS x]
  shrink (MetersPerSec _) = []


data SpeedRating = TurtleSlow
                 | Slow
                 | Average
                 | Fast
                 | RabbitFast
                 deriving (Show)

speedRatings :: [SpeedRating]
speedRatings = [TurtleSlow, Slow, Average, Fast, RabbitFast]

speedToRating :: Sport -> Speed -> SpeedRating
speedToRating sport speed = select speed (pickDtable sport) speedRatings
  where
    pickDtable Ride = KmPerHour <$> [15, 19, 22, 30]
    pickDtable Run = MinPerKm <$> [12.0, 10.0, 7.0, 5.0]
    pickDtable Walk = MinPerKm <$> [20.0, 15.0, 12.0, 10.0]
    pickDtable Hike = MinPerKm <$> [20.0, 15.0, 12.0, 10.0]

    {-pickDtable Hike =-}
    {-pickDtable AlpineSki =-}
    {-pickDtable Golf = Kilometers <$> [0, 0, 0, 0, 0]-}
