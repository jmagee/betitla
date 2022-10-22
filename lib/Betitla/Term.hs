-- | Typeclass for Terms.
{-# LANGUAGE LambdaCase #-}

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
import           Path                   (parseAbsFile)

type ReaderIO a b = ReaderT a IO b

-- | Term class.
class (FromJSON a, Eq a) => Term a where
  termFile :: a -> String
  pickTerm :: a -> ReaderIO Env String
  pickTerm rating = asks (M.lookup (termFile rating)) >>= \case
      Just x  -> liftIO $ parseAbsFile x >>= readTermTable >>= \y -> pickRandTerm rating y
      Nothing -> pure "nada"
