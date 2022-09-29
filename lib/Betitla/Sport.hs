{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes   #-}

module Betitla.Sport
( Sport (..)
) where

import           Betitla.Term
import           Betitla.Util

import           Data.Aeson                (FromJSON)
import           GHC.Generics              (Generic)
import           Path                      (Abs, File, Path, absfile)
import           Test.QuickCheck           (Gen, oneof)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary, shrink)

data Sport = AlpineSki
           | BackcountrySki
           | Canoeing
           | Crossfit
           | EBikeRide
           | Elliptical
           | Golf
           | Handcycle
           | Hike
           | IceSkate
           | InlineSkate
           | Kayaking
           | Kitesurf
           | NordicSki
           | Ride
           | RockClimbing
           | RollerSki
           | Rowing
           | Run
           | Sail
           | Skateboard
           | Snowboard
           | Snowshoe
           | Soccer
           | StairStepper
           | StandUpPaddling
           | Surfing
           | Swim
           | Velomobile
           | VirtualRide
           | VirtualRun
           | Walk
           | WeightTraining
           | Wheelchair
           | Windsurf
           | Workout
           | Yoga
           deriving (Show, Generic, Eq)

instance Term Sport where
  --termFile = const [absfile|/Users/jmagee/src/betitla.git/Sport.terms|]
  termFile = const "Sport.terms"
instance FromJSON Sport
