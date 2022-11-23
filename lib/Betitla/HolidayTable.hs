{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Betitla.HolidayTable
( HolidayTableEntry (..)
, HolidayTable
, readHolidayTable
, pickRandNoun
, pickRandPrefix
, lookupHoliday
, isHoliday
, holidays'
, holidays

-- Lenses
, holiday
, recurring
, nouns
, prefices
) where

import           Betitla.Env
import           Betitla.Error
import           Betitla.Time
import           Betitla.Util

import           Control.Lens.Getter    ((^.))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT, asks)
import           Data.Aeson             (FromJSON, eitherDecode')
import           Data.Functor           ((<&>))
import           Data.Map               as M (lookup)
import           Data.Sequence          (Seq (..))
import qualified Data.Sequence          as S (filter, null)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           Path                   (Abs, File, Path, parseAbsFile)
import           Witch                  (from)

-- FIXME
type ReaderIO a b = ReaderT a IO b

data HolidayTableEntry =
  --HolidayTableEntry { _holiday    :: Holiday
  HolidayTableEntry { _holiday    :: Text
                    , _calday     :: CalDay
                    , _recurring  :: Bool
                    , _nouns      :: Seq Text
                    , _prefices   :: Seq Text
                    } deriving (Generic, Show)

instance FromJSON HolidayTableEntry

-- | holiday lens.
holiday :: Functor f
      => (Text -> f Text )
      -> (HolidayTableEntry -> f HolidayTableEntry)
holiday zoom (HolidayTableEntry a b c d e) =
  fmap (\new -> HolidayTableEntry new b c d e) (zoom a)

-- | calday lens.
calday :: Functor f
      => (CalDay -> f CalDay)
      -> (HolidayTableEntry -> f HolidayTableEntry)
calday zoom (HolidayTableEntry a b c d e) =
  fmap (\new -> HolidayTableEntry a new c d e) (zoom b)

-- | recurring lens.
recurring :: Functor f
      => (Bool -> f Bool)
      -> (HolidayTableEntry -> f HolidayTableEntry)
recurring zoom (HolidayTableEntry a b c d e) =
  fmap (\new -> HolidayTableEntry a b new d e) (zoom c)

-- | nouns lens.
nouns :: Functor f
      => (Seq Text -> f (Seq Text))
      -> (HolidayTableEntry -> f HolidayTableEntry)
nouns zoom (HolidayTableEntry a b c d e) =
  fmap (\new -> HolidayTableEntry a b c new e) (zoom d)

-- prefices lens.
prefices :: Functor f
         => (Seq Text -> f (Seq Text))
         -> (HolidayTableEntry -> f HolidayTableEntry)
prefices zoom (HolidayTableEntry a b c d e) =
  HolidayTableEntry a b c d <$> zoom e

type HolidayTable = Seq HolidayTableEntry

-- | Read a Holiday from the provided absolute file path.
readHolidayTable:: Path Abs File -> IO HolidayTable
readHolidayTable file = unlessEmpty file Empty $ \contents ->
  either (jbail file) id (eitherDecode' contents :: Either String HolidayTable)

-- | Pick any nouns, at random, from the holiday table entry.
-- If there are no synonyms, then this will return the string "nothing".
pickRandNoun :: HolidayTableEntry -> IO Text
pickRandNoun table = pickRandText $ table ^. nouns

-- | Pick any prefices, at random, from the holiday table entry.
pickRandPrefix:: HolidayTableEntry -> IO Text
pickRandPrefix table = pickRandText $ table ^. prefices

-- | Lookup a holiday table entry
lookupHoliday :: CalDay -> HolidayTable -> Seq HolidayTableEntry
lookupHoliday cal = S.filter isSameDay
  where
    isSameDay entry = case (entry ^. calday) !==! cal of
      (True, True, True)  -> True
      (False, True, True) -> entry ^. recurring
      _                   -> False

-- | Determine if a day is a holiday
isHoliday :: CalDay -> HolidayTable -> Bool
isHoliday = notEmpty . lookupHoliday
 where
   notEmpty = ((not . S.null) .)

-- | Get all the holidays.
-- This version returns an empty table upon failure.
holidays' :: ReaderIO Env HolidayTable
holidays' = asks (M.lookup "Hols.holidays") >>= \case
  Just x   -> liftIO $ parseAbsFile (from x) >>= readHolidayTable
  Nothing  -> pure Empty

-- | Get all the holidays.
-- This version returns a Left Error upon failure.
holidays :: ReaderIO Env (Either Error HolidayTable)
holidays = holidays' <&> \case
    Empty -> Left $ ConfigError "No holiday file found"
    yummy -> Right yummy
