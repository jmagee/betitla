module StravaRenamer.Sentence
( Sentence (..)
) where

import           StravaRenamer.ActivityRating

-- Sentence types
-- "Epic ride .."
-- Holiday activity
-- "Long, flat, and fast activity"
data Sentence = SentenceThree
--              | SentenceHoliday
              deriving (Show)


genSentence :: ActivityRating -> Sentence -> IO String
genSentence activity SentenceThree = undefined
