-- | Typeclass for Terms.

module Betitla.Term
( Term
, pickRatingTerm
) where

import           Betitla.TermTable

import           Data.Aeson        (FromJSON)
import           Path              (Abs, File, Path)

-- | Term class.
class (FromJSON a, Eq a) => Term a

pickRatingTerm :: Term a => Path Abs File -> a -> IO String
pickRatingTerm file rating = readTermTable file >>= pickRandTerm rating
