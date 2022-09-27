{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Betitla.TermTable
( TermTableEntry (..)
, TermTable
, readTermTable
, pickRandTerm
) where

import           GHC.Generics

import           Betitla.Util

import           Data.Aeson    (FromJSON, Value (..), eitherDecode', parseJSON,
                                (.:))
import           Data.Maybe    (fromMaybe)
import           Data.Sequence (Seq (..), findIndexL)
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

-- | Pick a random term from a term table.
pickRandTerm :: Eq a => a -> TermTable a -> IO String
pickRandTerm key table = maybe (pure "nothing") pickRandSyn $
  findIndexL ((key ==) . _term) table >>= (flip S.lookup) table

readTermTable :: FromJSON a => Path Abs File -> IO (TermTable a)
readTermTable file = unlessEmpty file Empty $ \contents ->
  either (jbail file) id (eitherDecode' contents :: FromJSON a => Either String (TermTable a))

{-pickRandTerm' :: Eq a => a -> TermTable a -> IO String-}
{-pickRandTerm' key table = do-}
  {-x <- runMaybeT $ (lift.pure) (indexInto table) >>= \z -> (lift.pure) ((flip S.lookup) table z) >>= \x -> pickRandSyn' x-}
  {-case x of-}
    {-Nothing  -> pure "nothing"-}
    {-Just x   -> pure x-}
  {-where-}
    {-indexInto = findIndexL ((key ==) . _term)-}

pickRandSyn :: TermTableEntry a -> IO String
pickRandSyn (TermTableEntry _  syns) = do
  gen <- getStdGen
  let r  = fst $ randomR (0, S.length syns - 1) gen
  pure $ fromMaybe "nothing" (S.lookup r syns)

{-pickRandSyn' :: TermTableEntry a -> MaybeIO String-}
{-pickRandSyn' (TermTableEntry _  syns) = MaybeT $ do-}
  {-gen <- getStdGen-}
  {-let r  = fst $ randomR (0, S.length syns) gen-}
  {-pure $ S.lookup r syns-}
  {---pure $ fromMaybe "nothing" (S.lookup r syns)-}
