module StravaRenamer.Util
( select
, pickAny
) where

import           Data.List             (findIndex)

import           System.Random         (getStdGen)
import           System.Random.Shuffle (shuffle')

-- | Use a table of ordinal things to select the thing ratings.
--select :: Distance -> [Distance] -> [DistanceRating] -> DistanceRating
select :: Ord a => a -> [a] -> [b] -> b
select dist dtable rtable = maybeIndex rtable $ findIndex (dist <=) dtable where
    maybeIndex = maybe def . (!!)
    def = last rtable

-- | Pick an element from a list at random.
pickAny :: [a] -> IO a
pickAny xs = head <$> shuffle' xs (length xs) <$> getStdGen
