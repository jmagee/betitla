{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

module Betitla.StriverIds
( AthleteId (..)
, ActivityId (..)
, athleteIdFromInteger
) where

import           Data.Int (Int64)
import           Witch    (From, from, unsafeInto)

newtype AthleteId = AthleteId Int64
                  deriving (Show)

instance From AthleteId Int64 where
  from (AthleteId x) = x

athleteIdFromInteger :: Integer -> AthleteId
athleteIdFromInteger x = AthleteId $ unsafeInto x

newtype ActivityId = ActivityId Int64
                   deriving (Show)

instance From ActivityId Int64 where
  from (ActivityId x) = x

instance From ActivityId Integer where
  from (ActivityId x) = from x
