{-# LANGUAGE OverloadedStrings #-}
module Betitla.Error
( Error (..)
, errorForNothing
) where

import           Betitla.Display

import           Data.Text       (Text)

data Error = StriveError Text
           | DBDuplicateError Text
           | DBNotFoundError Text
           | DBError Text
           | BErrorNotNew Text
           | BErrorPurse Text
           | ConfigError Text
           deriving (Show, Eq)

instance Display Error where
  display (StriveError s) = "Error when communicating with remote API: " <> s
  display (DBDuplicateError s) = "Duplicate entry in database: " <> s
  display (DBNotFoundError s) = "Entry not found in database: " <> s
  display (DBError s) = "Database error: " <> s
  display (BErrorNotNew s) = "Betitla error, activity has already been processed: " <> s
  display (BErrorPurse s) = "Betitla error.  I don't know you, that's my purse: " <> s
  display (ConfigError s) = "Betitla configuration error: " <> s

-- | Convert a Maybe to an Either.
errorForNothing :: Error -> Maybe a -> Either Error a
errorForNothing e Nothing = Left e
errorForNothing _ (Just x) = Right x
