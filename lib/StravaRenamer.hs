module StravaRenamer
( rename
, localTimeToPOD
) where

import           Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))

data PhaseOfDay = PreDawn
                | Morning
                | Afternoon
                | Evening
                | LateNight
                deriving (Show)

localTimeToPOD :: LocalTime -> PhaseOfDay
localTimeToPOD (LocalTime _ (TimeOfDay hour _ _))
  | hour < 3  = LateNight
  | hour < 6  = PreDawn
  | hour < 12 = Morning
  | hour < 18 = Afternoon
  | hour < 9  = Evening
  | otherwise = LateNight

rename :: IO String
rename = pure "blah"
