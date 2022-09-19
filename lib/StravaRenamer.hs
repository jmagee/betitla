module StravaRenamer
( rename
, localTimeToPOD
) where

import           StravaRenamer.Time

import           Data.Time           (NominalDiffTime)

{-data Activity =-}
  {-Activity { _sport      :: Sport-}
           {-, _distance   :: Distance-}
           {-, _movingTime :: NominalDiffTime-}
           {-, _elevation  :: Distance-}
           {-, -}


rename :: IO String
rename = pure "blah"
