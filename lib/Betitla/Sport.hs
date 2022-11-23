{-# LANGUAGE DeriveGeneric #-}

module Betitla.Sport
( Sport (..)
) where

import           Betitla.Term

import           Data.Aeson   (FromJSON)
import           GHC.Generics (Generic)

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
  termFile = const "Sport.terms"
instance FromJSON Sport
