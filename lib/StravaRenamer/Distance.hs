module StravaRenamer.Distance
( Distance (..)
, distanceToRating
, kmToMeters
, metersToKm
) where

import           StravaRenamer.Sport

import           Test.QuickCheck           (Gen, oneof)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary, shrink)


data Distance = Meters Int
              | Kilometers Int
              deriving (Show)

metersToKm :: Distance -> Distance
metersToKm (Meters m) = Kilometers $ m `div` 1000
metersToKm km = km

kmToMeters :: Distance -> Distance
kmToMeters (Kilometers km) = Meters $ km * 1000
kmToMeters m = m

instance Eq Distance where
  (Meters m) == (Kilometers km)     = m == km * 1000
  km@(Kilometers _) == m@(Meters _) = m == km
  (Meters m) == (Meters m2)         = m == m2
  (Kilometers km) == (Kilometers km2) = km == km2

instance Ord Distance where
  compare m@(Meters _) km@(Kilometers _)   = compare m $ kmToMeters km
  compare km@(Kilometers _)  m@(Meters _)  = compare (kmToMeters km) m
  compare (Meters m) (Meters m2)           = compare m m2
  compare (Kilometers km) (Kilometers km2) = compare km km2

data DistanceRating = VeryShort
                    | Short
                    | Medium
                    | Far
                    | VeryFar
                    | InsaneFar
                    deriving (Show)

-- | Convert a distance into a DistanceRating.
-- The conversion depends on the Sport type, since 20km is an insane distance
-- for a swim, a pretty far distance for a run, and a relatively short distance for
-- a ride.
distanceToRating :: Sport -> Distance -> DistanceRating
distanceToRating Ride dist
  | dist <= Kilometers 10 = VeryShort
  | dist <= Kilometers 20 = Short
  | dist <= Kilometers 60 = Medium
  | dist <= Kilometers 130 = Far
  | dist <= Kilometers 250 = VeryFar
  | otherwise              = InsaneFar

{-distanceToRating Run m@(Meters _) = distanceToRating Run $ metersToKm m-}
{-distanceToRating Run (Kilometers k)-}
  {-| k <= 3 = VeryShort-}

instance Arbitrary Distance where
  arbitrary = oneof [Meters <$> (arbitrary :: Gen Int), Kilometers <$> (arbitrary :: Gen Int)]
  shrink x@(Kilometers _) = [kmToMeters x]
  shrink (Meters _) = []
