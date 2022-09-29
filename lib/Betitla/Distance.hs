{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes   #-}

module Betitla.Distance
( Distance (..)
, DistanceRating (..)
, Elevation (..)
, ElevationRating (..)
, distanceToRating
, elevationToRating
, toMeters
, toKm
--, pickDistanceRatingTerm
--, pickElevationRatingTerm
) where

import           GHC.Generics

import           Betitla.Sport
import           Betitla.Term
import           Betitla.TermTable
import           Betitla.Util

import           Data.Aeson                (FromJSON)
import           Path                      (Abs, File, Path, absfile)
import           Test.QuickCheck           (Gen, oneof)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary, shrink)

data Distance = Meters Int
              | Kilometers Int
              deriving (Show)

toKm :: Distance -> Distance
toKm (Meters m) = Kilometers $ m `div` 1000
toKm km = km

toMeters :: Distance -> Distance
toMeters (Kilometers km) = Meters $ km * 1000
toMeters m = m

instance Eq Distance where
  (Meters m) == (Meters m2) = m == m2
  x          == y           = toMeters x == toMeters y

instance Ord Distance where
  compare (Meters m) (Meters m2) = compare m m2
  compare x          y           = compare (toMeters x) (toMeters y)

instance Arbitrary Distance where
  arbitrary = oneof [Meters <$> (arbitrary :: Gen Int), Kilometers <$> (arbitrary :: Gen Int)]
  shrink x@(Kilometers _) = [toMeters x]
  shrink (Meters _) = []

data DistanceRating = VeryNear
                    | Near
                    | Medium
                    | Far
                    | VeryFar
                    | InsaneFar
                    deriving (Show, Eq, Generic)

instance Term DistanceRating where
  --termFile = const [absfile|/Users/jmagee/src/betitla.git/DistanceRating.terms|]
  termFile = const "DistanceRating.terms"
instance FromJSON DistanceRating
instance Arbitrary DistanceRating where
  arbitrary = oneof $ pure <$> distanceRatings

distanceRatings :: [DistanceRating]
distanceRatings = [VeryNear, Near, Medium, Far, VeryFar, InsaneFar]

-- | Convert a distance into a DistanceRating.
-- The conversion depends on the Sport type, since 20km is an insane distance
-- for a swim, a pretty far distance for a run, and a relatively short distance for
-- a ride.
distanceToRating :: Sport -> Distance -> DistanceRating
distanceToRating sport dist = select dist (pickDtable sport) distanceRatings
  where
    pickDtable Ride = Kilometers <$> [10, 20, 60, 130, 250]
    pickDtable Run  = Kilometers <$> [3, 5, 10, 22, 43]
    pickDtable Walk = Kilometers <$> [1, 2, 5, 10, 20]
    pickDtable Hike = pickDtable Run
    pickDtable AlpineSki = Kilometers <$> [1, 5, 10, 20, 30]
    pickDtable Golf = Kilometers <$> [0, 0, 0, 0, 0]

-- | Pick a DistanceRating from the DistanceRating.terms file which is read at runtime.
--pickDistanceRatingTerm :: DistanceRating -> IO String
--pickDistanceRatingTerm = pickRatingTerm [absfile|/Users/jmagee/src/betitla.git/DistanceRating.terms|]

data Elevation = MetersGained Int
               deriving (Show, Eq, Ord)

instance Arbitrary Elevation where
  arbitrary = MetersGained <$> (arbitrary :: Gen Int)

data ElevationRating = PancakeFlat
                     | Flat
                     | Hilly
                     | VeryHilly
                     | SuperGoat
                     deriving (Show, Eq, Generic)

instance Term ElevationRating where
  --termFile = const [absfile|/Users/jmagee/src/betitla.git/ElevationRating.terms|]
  termFile = const "ElevationRating.terms"
instance FromJSON ElevationRating
instance Arbitrary ElevationRating where
  arbitrary = oneof $ pure <$> elevationRatings

elevationRatings :: [ElevationRating]
elevationRatings = [PancakeFlat, Flat, Hilly, VeryHilly, SuperGoat]

elevationToRating :: Sport -> Elevation -> Distance -> ElevationRating
elevationToRating sport elevation dist = select (metersPerKm elevation dist) (pickDtable sport) elevationRatings
  where
    metersPerKm (MetersGained m) (Kilometers km) = m `div` km
    metersPerKm x y = metersPerKm x (toKm y)
    pickDtable Ride = [1, 4, 10, 20]

-- | Pick a DistanceRating from the DistanceRating.terms file which is read at runtime.
--pickElevationRatingTerm :: ElevationRating -> IO String
--pickElevationRatingTerm = pickRatingTerm [absfile|/Users/jmagee/src/betitla.git/ElevationRating.terms|]
