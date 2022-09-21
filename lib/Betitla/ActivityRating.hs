module Betitla.ActivityRating
( ActivityRating (..)
) where

import           Betitla.Distance
import           Betitla.Speed
import           Betitla.Sport
import           Betitla.Time

data ActivityRating =
  ActivityRating { _sport           :: Sport
                 , _distanceRating  :: DistanceRating
                 , _durationRating  :: DurationRating
                 , _elevationRating :: ElevationRating
                 , _speedRating     :: SpeedRating
                 }

