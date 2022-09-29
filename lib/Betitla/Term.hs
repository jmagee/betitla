-- | Typeclass for Terms.

{-# LANGUAGE LambdaCase #-}
module Betitla.Term
( Term
, termFile
, pickTerm
--, pickRatingTerm
) where

import           Betitla.Env
import           Betitla.TermTable

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (Reader, ReaderT, ask)
import           Data.Aeson             (FromJSON)
import qualified Data.Map               as M (lookup)
import           Path                   (Abs, File, Path, parseAbsFile)

type ReaderIO a b = ReaderT a IO b

-- | Term class.
class (FromJSON a, Eq a) => Term a where
  termFile :: a -> String
  --termFile = undefined
  pickTerm :: a -> ReaderIO Env String
  pickTerm rating = M.lookup (termFile rating) <$> ask >>= \case
      Just x  -> liftIO $ parseAbsFile x >>= readTermTable >>= \y -> pickRandTerm rating y
      Nothing -> pure "nada"


--readTermTable (pure $ termFile rating) >>= pickRandTerm rating


--pickRatingTerm :: Term a => Path Abs File -> a -> IO String
--pickRatingTerm file rating = readTermTable file >>= pickRandTerm rating
