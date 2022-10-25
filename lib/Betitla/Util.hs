module Betitla.Util
( select
, jbail
, unlessEmpty
, pickAny
, dropAny
, mkHomePath
, randInt
, coinFlip
, tshow
, (?)
, eitherTGuard_
) where

import           Control.Monad.Trans.Either (EitherT, hoistEither)
import           Data.Bool                  (bool)
import qualified Data.ByteString.Lazy       as BS (ByteString, null, readFile)
import           Data.Functor               ((<&>))
import           Data.List                  (findIndex)
import           Data.Text                  (Text)
import           Path                       (Abs, File, Path, toFilePath)
import           System.Directory           (doesFileExist, getHomeDirectory)
import           System.FilePath.Posix      (pathSeparator)
import           System.Random              (getStdGen, randomR)
import           System.Random.Shuffle      (shuffle')
import           Witch                      (into)


-- | Use a table of ordinal things to select the thing ratings.
--select :: Distance -> [Distance] -> [DistanceRating] -> DistanceRating
select :: Ord a => a -> [a] -> [b] -> b
select dist dtable rtable = maybeIndex rtable $ findIndex (dist <=) dtable where
    maybeIndex = maybe def . (!!)
    def = last rtable

-- | Pick an element from a list at random.
pickAny :: [a] -> IO a
pickAny xs = head . shuffle' xs (length xs) <$> getStdGen

-- | Drop any element from a list at random
dropAny :: Int -> [a] -> IO [a]
dropAny n xs = drop n . shuffle' xs (length xs) <$> getStdGen

-- | Read the file, unless it is empty, in which case return a default value.
unlessEmpty :: Path b File -> a -> (BS.ByteString -> a) -> IO a
unlessEmpty file def action =
  let f = toFilePath file
  in doesFileExist f >>= bool (pure def) (go <$> BS.readFile f)
  where
    go contents
      | BS.null contents = def
      | otherwise = action contents

-- | Bail out error when parsing a json file.
jbail :: Path Abs File -> String -> a
jbail f e = error $ "Error parsing JSON file <" ++ toFilePath f ++ ">:" ++ e

-- | Insert a system specific path separator
slash :: FilePath -> FilePath -> FilePath
slash a b = a ++ [pathSeparator] ++ b

-- | Create a patch in the home directory
mkHomePath :: FilePath -> IO FilePath
mkHomePath x = (`slash` x) <$> getHomeDirectory

-- | Generate a random integer between n and m.
randInt :: Int -> Int -> IO Int
randInt n m = getStdGen <&> (fst . randomR (n, m))

-- | Flip for it.
coinFlip :: IO Bool
--coinFlip = randInt 0 1 >>= pure . (== 1)
coinFlip = randInt 0 1 <&> (==1)

-- | Show for Text
tshow :: Show a => a -> Text
tshow = into . show

-- | Functional alternative to if-then-else.
-- See https://wiki.haskell.org/If-then-else
if' :: Bool -> a -> a -> a
if' True x _  = x
if' False _ y = y

-- | C-like ternary operator
infixr 1 ?
(?) :: Bool -> a -> a -> a
(?) = if'

-- | EitherT guard
-- Conditional failure in the EitherT monad transformer , using the provided
-- error as the failure.
eitherTGuard_ :: Monad m => x -> Bool -> EitherT x m ()
eitherTGuard_ err = hoistEither . bool (Left err) (pure ())
