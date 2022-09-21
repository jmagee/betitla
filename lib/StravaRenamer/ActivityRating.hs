module StravaRenamer.ActivityRating
( ActivityRating (..)
) where

import           StravaRenamer.Distance
import           StravaRenamer.Speed
import           StravaRenamer.Sport
import           StravaRenamer.Time

data ActivityRating =
  ActivityRating { _sport           :: Sport
                 , _distanceRating  :: DistanceRating
                 , _durationRating  :: DurationRating
                 , _elevationRating :: ElevationRating
                 , _speedRating     :: SpeedRating
                 }

