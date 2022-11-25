{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Betitla.Sentence
( Sentence (..)
, genSentence
, genSentence'
, intercalateAnd
, getAll
, lookupTest
, lookupTest'
) where

import           Betitla.ActivityRating
--import           Betitla.Distance
import           Betitla.Env
--import           Betitla.Speed
--import           Betitla.Sport
import           Betitla.Term
import           Betitla.HolidayTable
--import           Betitla.Time
import           Betitla.Util

import           Control.Lens.Getter    ((^.))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (Reader, ReaderT, ask, runReaderT)
import           Data.Char              (isSpace, toUpper)
import           Data.List              (intersperse)
import qualified Data.Map               as M (lookup)
import           Data.Sequence          (Seq (..))
import           Data.Text              (Text)
import qualified Data.Text              as T (concat, cons, dropWhile, uncons,
                                              unwords)

-- Sentence types
-- "Epic ride .."
-- Holiday activity
-- "Long, flat, and fast activity"
data Sentence = SentenceSimple
              | SentenceHoliday
              deriving (Show)

type ReaderIO a b = ReaderT a IO b

lookupTest :: Text -> Reader Env (Maybe Text)
lookupTest s = ask >>= \x -> pure $ M.lookup s x

lookupTest' :: Text -> ReaderIO Env (Maybe Text)
lookupTest' s = ask >>= \x -> pure $ M.lookup s x

genSentence' :: Sentence -> ActivityRating -> IO Text
genSentence' sentence rating = getEnvRC >>= runReaderT (genSentence sentence rating)

genSentence :: Sentence -> ActivityRating -> ReaderIO Env Text
genSentence SentenceSimple rating = do
  phase <- liftIO coinFlip >>= \x -> x ? pickTerm (rating ^. phaseOfDay) $ pure ""
  terms <- liftIO (randInt 3 4) >>= \r -> genN r rating
  sprt  <- pickTerm $ rating ^. sport
  (pure . capFirst . dropLeadingSpaces ) $ T.unwords [phase, terms, sprt]

--genSentence SentenceHoliday _ = undefined
genSentence SentenceHoliday rating = do
  sprt      <- pickTerm $ rating ^. sport
  celebrate <- lookupHoliday (rating ^. calenderDay) <$> holidays'

  case celebrate of
    Empty -> genSentence SentenceSimple rating
    (x :<| _)    -> liftIO coinFlip >>= \case
      True -> do -- Noun variant
        noun  <- liftIO $ pickRandNoun x
        (pure . capFirst) $ T.unwords [noun, sprt]
      False -> do
        noun  <- liftIO $ pickRandPrefix x
        terms <- liftIO (randInt 2 3) >>= \r -> genN r rating
        (pure . capFirst) $ T.unwords [noun, terms, sprt]

getAll :: ActivityRating -> ReaderIO Env [Text]
getAll activity = sequence
  [ --pickTerm phase
    pickTerm $ activity ^. speedRating
  , pickTerm $ activity ^. elevationRating
  , pickTerm $ activity ^. durationRating
  , pickTerm $ activity ^. distanceRating
  --, pickTerm sport
  ]

genN :: Int -> ActivityRating -> ReaderIO Env Text
genN n activity = getAll activity >>= sentency . drop m
  where
    m = 4 - n
    sentency = pure . intercalateAnd ", " ", and "

-- | Like intercalate but with a separate pattern for the last items.
-- I.e. "a, b, and c".
intercalateAnd :: Text -> Text -> [Text] -> Text
intercalateAnd join1 join2 list = T.concat $ intersperse join1 (removeTail list) ++ [join2, last list]

-- Like intercalateAnd, but with an additional join pattern.
-- I.e. "a, b, and c; but d"
{-intercalateAndAnd :: [a] -> [a] -> [a] -> [[a]] -> [a]-}
{-intercalateAndAnd join1 join2 join3 list = intercalateAnd join1 join2 (removeTail list) ++ concat [join3, last list]-}

-- | Remove the tail of a list.
-- Many ways to skin this cat.  This is not the most effecient.
removeTail :: [a] -> [a]
removeTail = reverse . drop 1 . reverse

-- | Cap first
--capFirst :: [Char] -> [Char]
--capFirst (x:xs) = toUpper x : xs
--capFirst []     = []
capFirst :: Text -> Text
capFirst t = case T.uncons t of
  Nothing      -> t
  Just (x, xs) -> T.cons (toUpper x) xs

-- | Drop leading whitespace from a string
dropLeadingSpaces :: Text -> Text
dropLeadingSpaces = T.dropWhile isSpace

{--- | Apply something to the start of the sentence-}
{-applyPrefix :: [Char] -> [Char] -> [Char]-}
{-applyPrefix prefix sentence = prefix ++ " " ++ sentence-}

{--- | Add sport type-}
{-applySport  :: [Char] -> [Char] -> [Char]-}
{-applySport sport sentence = sentence ++ " " ++ sport-}
