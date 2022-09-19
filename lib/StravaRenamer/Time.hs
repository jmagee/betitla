module StravaRenamer.Time
( PhaseOfDay (..)
, localTimeToPOD
, podTerms
) where

import           Data.Time.LocalTime (LocalTime (..), TimeOfDay (..))

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

podTerms :: PhaseOfDay -> [String]
podTerms PreDawn = ["before the sunrise", "bleeding early", "before the dawn", "while the world sleeps"]
podTerms Morning = ["morning", "ante meridiem", "early"]
podTerms Afternoon = ["afternoon", "midday"]
podTerms Evening = ["evening", "after dinner"]
podTerms LateNight = ["night", "late", "midnight", "shadow"]
