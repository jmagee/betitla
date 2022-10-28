{-# LANGUAGE OverloadedStrings #-}
module Betitla.Error
( Error (..)
, errorFromString
, errorForNothing
, errorForNothing'
) where

import           Betitla.Display

import           Data.String.Conversions (cs)
import           Data.Text               (Text, append)


data Error = StriveError Text
           | DBDuplicateError Text
           | DBNotFoundError Text
           | DBError Text
           | BErrorNotNew Text
           | BErrorPurse Text
           deriving (Show)

instance Display Error where
  display (StriveError s) = "Error when communicating with remote API: " `append` s
  display (DBDuplicateError s) = "Duplicate entry in database: " `append` s
  display (DBNotFoundError s) = "Entry not found in database: " `append` s
  display (DBError s) = "Database error: " `append` s
  display (BErrorNotNew s) = "Betitla error, activity has already been processed: " `append` s
  display (BErrorPurse s) = "Betitla error.  I don't know you, that's my purse: " `append` s

errorFromString :: (Text -> Error) -> String -> Error
errorFromString cons = cons . cs

-- | Convert a Maybe to an Either.
errorForNothing :: (Text -> Error) -> Text -> Maybe a -> Either Error a
errorForNothing cons msg Nothing = (Left . cons) msg
errorForNothing _    _   (Just x) = Right x

-- | Convert a Maybe to an Either.
errorForNothing' :: e -> Maybe a -> Either e a
errorForNothing' e Nothing = Left e
errorForNothing' _ (Just x) = Right x
