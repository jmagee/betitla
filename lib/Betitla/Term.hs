-- | Typeclass for Terms.
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Betitla.Term
( Term
, termFile
, pickTerm
) where

import           Betitla.Env
import           Betitla.TermTable

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT, asks)
import           Data.Aeson             (FromJSON)
import qualified Data.Map               as M (lookup)
import           Data.Text              (Text)
import           Path                   (parseAbsFile)
import Witch (from)

type ReaderIO a b = ReaderT a IO b

-- | Term class.
class (FromJSON a, Eq a) => Term a where
  termFile :: a -> Text
  pickTerm :: a -> ReaderIO Env Text
  pickTerm rating = asks (M.lookup (termFile rating)) >>= \case
      Just x  -> liftIO $ parseAbsFile (from x) >>= readTermTable >>= pickRandTerm rating
      Nothing -> pure "nada"
