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
import qualified Data.Sequence as S (length, lookup)
import           GHC.Generics  (Generic)
import           Path          (Abs, File, Path)

data TermTableEntry a =
  TermTableEntry { _term     :: a
                 , _synonyms :: Seq String
                 } deriving (Generic, Show)

instance FromJSON a => FromJSON (TermTableEntry a)

type TermTable a = Seq (TermTableEntry a)

-- | Pick a random term from a term table.
pickRandTerm :: Eq a => a -> TermTable a -> IO String
pickRandTerm key table = maybe (pure "nothing") pickRandSyn $
  findIndexL ((key ==) . _term) table >>= flip S.lookup table

readTermTable :: FromJSON a => Path Abs File -> IO (TermTable a)
readTermTable file = unlessEmpty file Empty $ \contents ->
  either (jbail file) id (eitherDecode' contents :: FromJSON a => Either String (TermTable a))

pickRandSyn :: TermTableEntry a -> IO String
pickRandSyn (TermTableEntry _  syns) = do
  r <- randInt 0 $ S.length syns - 1
  pure $ fromMaybe "nothing" (S.lookup r syns)
