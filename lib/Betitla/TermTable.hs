{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Betitla.TermTable
( TermTableEntry (..)
, TermTable
, readTermTable
, pickRandTerm
) where

import           Betitla.Util

import           Data.Aeson    (FromJSON, eitherDecode')
import           Data.Maybe    (fromMaybe)
import           Data.Sequence (Seq (..), findIndexL)
import qualified Data.Sequence as S (lookup)
import           Data.Text     (Text)
import           GHC.Generics  (Generic)
import           Path          (Abs, File, Path)

data TermTableEntry a =
  TermTableEntry { _term     :: a
                 , _synonyms :: Seq Text
                 } deriving (Generic, Show)

instance FromJSON a => FromJSON (TermTableEntry a)

type TermTable a = Seq (TermTableEntry a)

-- | Pick a random term from a term table.
pickRandTerm :: Eq a => a -> TermTable a -> IO Text
pickRandTerm key table = maybe (pure "nothing") pickRandSyn $
  findIndexL ((key ==) . _term) table >>= flip S.lookup table

-- | Read a TermTable from the provided absolute file path.
readTermTable :: FromJSON a => Path Abs File -> IO (TermTable a)
readTermTable file = unlessEmpty file Empty $ \contents ->
  either (jbail file) id (eitherDecode' contents :: FromJSON a => Either String (TermTable a))

-- | Pick any synonym, at random, from the term table entry.
-- If there are no synonyms, then this will return the string "nothing".
pickRandSyn :: TermTableEntry a -> IO Text
pickRandSyn (TermTableEntry _ syns) = fromMaybe "nothing" <$> pickRand syns
