module Main (main) where

import Betitla

main :: IO ()
main = rename >>= putStrLn
