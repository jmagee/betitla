module Main (main) where

import StravaRenamer

main :: IO ()
main = rename >>= putStrLn
