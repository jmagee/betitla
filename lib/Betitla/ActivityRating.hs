{-# LANGUAGE TemplateHaskell #-}

module Betitla.ActivityRating
( ActivityRating (..)
, sport
, distanceRating
, durationRating
, elevationRating
, speedRating
, phaseOfDay
, calenderDay
) where

import           Betitla.Distance
import           Betitla.Speed
import           Betitla.Sport
import           Betitla.Time

import           Control.Lens    (makeLenses)

data ActivityRating =
  ActivityRating { _sport           :: Sport
                 , _distanceRating  :: DistanceRating
                 , _durationRating  :: DurationRating
                 , _elevationRating :: ElevationRating
                 , _speedRating     :: SpeedRating
                 , _phaseOfDay      :: PhaseOfDay
                 , _calenderDay     :: CalDay
                 } deriving (Show)

makeLenses ''ActivityRating
