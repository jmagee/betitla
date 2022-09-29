module Betitla.Sentence
( Sentence (..)
, genSentence
, intercalateAnd
, intercalateAndAnd
, getAll
, lookupTest
, lookupTest'
) where

import           Betitla.ActivityRating
import           Betitla.Distance
import           Betitla.Env
import           Betitla.Speed
import           Betitla.Sport
import           Betitla.Term
import           Betitla.Time
import           Betitla.Util

--import Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (Reader, ReaderT, ask)
import           Data.Char              (toUpper)
import           Data.List              (intersperse)
import qualified Data.Map as M (lookup)

-- Sentence types
-- "Epic ride .."
-- Holiday activity
-- "Long, flat, and fast activity"
data Sentence = SentenceFour
              | SentenceThree
              | SentenceTwo
--              | SentenceHoliday
              deriving (Show)

type ReaderIO a b = ReaderT a IO b

lookupTest :: String -> Reader Env (Maybe String)
lookupTest s = ask >>= \x -> pure $ M.lookup s x

lookupTest' :: String -> ReaderIO Env (Maybe String)
lookupTest' s = ask >>= \x -> pure $ M.lookup s x

genSentence :: Sentence -> ActivityRating -> ReaderIO Env String
genSentence SentenceTwo = genN 2
genSentence SentenceThree = genN 3
genSentence SentenceFour = genN 4

getAll :: ActivityRating -> ReaderIO Env [String]
getAll (ActivityRating sport distance duration elevation speed) = sequence
  [ pickTerm speed
  , pickTerm elevation
  , pickTerm duration
  , pickTerm distance
  , pickTerm sport
  ]

genN :: Int -> ActivityRating -> ReaderIO Env String
genN n activity = getAll activity >>= pure . drop m >>= sentency
  where
    m = 4 - n
    sentency = pure . capFirst . intercalateAndAnd ", " ", and " " "

-- | Like intercalate but with a separate pattern for the last items.
-- I.e. "a, b, and c".
intercalateAnd :: [a] -> [a] -> [[a]] -> [a]
intercalateAnd join1 join2 list = concat $ intersperse join1 (removeTail list) ++ [join2, last list]

-- Like intercalateAnd, but with an additional join pattern.
-- I.e. "a, b, and c; but d"
intercalateAndAnd :: [a] -> [a] -> [a] -> [[a]] -> [a]
intercalateAndAnd join1 join2 join3 list = intercalateAnd join1 join2 (removeTail list) ++ concat [join3, last list]

-- | Remove the tail of a list.
-- Many ways to skin this cat.  This is not the most effecient.
removeTail :: [a] -> [a]
removeTail = reverse . drop 1 . reverse

-- | Cap first
capFirst :: [Char] -> [Char]
capFirst (x:xs) = toUpper x : xs
capFirst []     = []
