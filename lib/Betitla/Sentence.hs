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
import           Betitla.Lenses
--import           Betitla.Speed
--import           Betitla.Sport
import           Betitla.Term
--import           Betitla.Time
import           Betitla.Util

import           Control.Lens.Getter    ((^.))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (Reader, ReaderT, ask, runReaderT)
import           Data.Char              (isSpace, toUpper)
import           Data.List              (intersperse)
import qualified Data.Map               as M (lookup)

-- Sentence types
-- "Epic ride .."
-- Holiday activity
-- "Long, flat, and fast activity"
data Sentence = SentenceSimple
--              | SentenceHoliday
              deriving (Show)

type ReaderIO a b = ReaderT a IO b

lookupTest :: String -> Reader Env (Maybe String)
lookupTest s = ask >>= \x -> pure $ M.lookup s x

lookupTest' :: String -> ReaderIO Env (Maybe String)
lookupTest' s = ask >>= \x -> pure $ M.lookup s x

genSentence' :: Sentence -> ActivityRating -> IO String
genSentence' sentence rating = getEnvRC >>= runReaderT (genSentence sentence rating)

genSentence :: Sentence -> ActivityRating -> ReaderIO Env String
genSentence SentenceSimple rating = do
  phase <- liftIO coinFlip >>= \x -> x ? pickTerm (rating ^. phaseOfDay) $ pure ""
  terms <- liftIO (randInt 3 4) >>= \r -> genN r rating
  sprt  <- pickTerm $ rating ^. sport
  (pure . capFirst . dropLeadingSpaces ) $ unwords [phase, terms, sprt]

getAll :: ActivityRating -> ReaderIO Env [String]
--getAll (ActivityRating sport distance duration elevation speed phase) = sequence
getAll activity = sequence
  [ --pickTerm phase
    pickTerm $ activity ^. speedRating
  , pickTerm $ activity ^. elevationRating
  , pickTerm $ activity ^. durationRating
  , pickTerm $ activity ^. distanceRating
  --, pickTerm sport
  ]

genN :: Int -> ActivityRating -> ReaderIO Env String
genN n activity = getAll activity >>= sentency . drop m
  where
    m = 4 - n
    sentency = pure . {-capFirst .-} intercalateAnd ", " ", and "

-- | Like intercalate but with a separate pattern for the last items.
-- I.e. "a, b, and c".
intercalateAnd :: [a] -> [a] -> [[a]] -> [a]
intercalateAnd join1 join2 list = concat $ intersperse join1 (removeTail list) ++ [join2, last list]

-- Like intercalateAnd, but with an additional join pattern.
-- I.e. "a, b, and c; but d"
{-intercalateAndAnd :: [a] -> [a] -> [a] -> [[a]] -> [a]-}
{-intercalateAndAnd join1 join2 join3 list = intercalateAnd join1 join2 (removeTail list) ++ concat [join3, last list]-}

-- | Remove the tail of a list.
-- Many ways to skin this cat.  This is not the most effecient.
removeTail :: [a] -> [a]
removeTail = reverse . drop 1 . reverse

-- | Cap first
capFirst :: [Char] -> [Char]
capFirst (x:xs) = toUpper x : xs
capFirst []     = []

-- | Drop leading whitespace from a string
dropLeadingSpaces :: String -> String
dropLeadingSpaces = dropWhile isSpace

{--- | Apply something to the start of the sentence-}
{-applyPrefix :: [Char] -> [Char] -> [Char]-}
{-applyPrefix prefix sentence = prefix ++ " " ++ sentence-}

{--- | Add sport type-}
{-applySport  :: [Char] -> [Char] -> [Char]-}
{-applySport sport sentence = sentence ++ " " ++ sport-}
