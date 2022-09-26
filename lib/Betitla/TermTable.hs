{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Betitla.TermTable
( TermTableEntry (..)
, TermTable
, readTermTable
, pickRandSyn
) where

import           GHC.Generics

import           Betitla.Util

import           Control.Monad (mzero)
import           Data.Aeson    (FromJSON, Value (..), eitherDecode', parseJSON,
                                (.:))
import           Data.Sequence (Seq (..))
import qualified Data.Sequence as S (length, lookup)
import           Path          (Abs, File, Path)
import           System.Random (getStdGen, randomR)

data TermTableEntry a =
  TermTableEntry { _term     :: a
                 , _synonyms :: Seq String
                 } deriving (Generic, Show)

instance FromJSON a => FromJSON (TermTableEntry a)

type TermTable a = Seq (TermTableEntry a)

--instance (Read a => (FromJSON (TermTableEntry a))) where
--instance FromJSON a => FromJSON (TermTableEntry a) where
{-instance FromJSON (TermTableEntry a) where-}
  {-parseJSON (Object v) =-}
    {-TermTableEntry <$> (read <$> (v .: "term") :: a)-}
              {-<*> v .: "synonyms"-}
  {-parseJSON _ = mzero-}

readTermTable :: FromJSON a => Path Abs File -> IO (TermTable a)
readTermTable file = unlessEmpty file Empty $ \contents ->
  either (jbail file) id (eitherDecode' contents :: FromJSON a => Either String (TermTable a))

-- | Pick a random term from a term table.
--pickRandTerm :: TermTable a -> a -> IO String
--pickRandTerm table key =

pickRandSyn :: TermTableEntry a -> IO String
pickRandSyn (TermTableEntry _  syns) = do
  gen <- getStdGen
  let (r, _) = randomR (0, S.length syns) gen
  case S.lookup r syns of
    Nothing -> pure "flabbergasted"
    Just x  -> pure x
