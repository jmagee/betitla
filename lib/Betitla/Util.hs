module Betitla.Util
( select
, jbail
, unlessEmpty
, pickAny
, dropAny
, mkHomePath
) where

import           Data.Bool             (bool)
import qualified Data.ByteString.Lazy  as BS (ByteString, null, readFile)
import           Data.List             (findIndex)
import           Path                  (Abs, File, Path, toFilePath)
import           System.Directory      (doesFileExist, getHomeDirectory)
import           System.FilePath.Posix (pathSeparator)
import           System.Random         (getStdGen)
import           System.Random.Shuffle (shuffle')

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
