module RenamerSpec (spec) where

import           StravaRenamer.Distance

import           Test.Hspec
import           Test.Hspec.QuickCheck  (prop)

spec :: Spec
spec = do

  describe "meters/kilometer conversions" $ do
    prop "a kilometer is one thousand meters" $ \x ->
      let check = x `div` 1000
      in metersToKm (Meters x) == Kilometers check
    prop "a meter is one thousandth of a kilometer" $ \x ->
      let check = x * 1000
      in kmToMeters (Kilometers x) == Meters check

  describe "Eq Distance" $ do
    prop "Reflexivity" $ \x ->
      let d = x :: Distance
      in d == d
    prop "Symmetry" $ \x y ->
      let a = x :: Distance
          b = y :: Distance
      in (a == b) == (a == b)
    prop "Transitivity" $ \x y z ->
      let a = x :: Distance
          b = y :: Distance
          c = z :: Distance
      in (not (a == b && b == c) || (a == c))
    prop "Substitutivity" $ \x y ->
      let a = x :: Distance
          b = y :: Distance
          f = id
      in if a == b
        then f a == f b
        else f a /= f b
    prop "Negation" $ \x y ->
      let a = x :: Distance
          b = y :: Distance
      in (a /= b) == not (a == b)

  describe "Ord Distance" $ do
    prop "Transitivity" $ \x y z ->
      let a = x :: Distance
          b = y :: Distance
          c = z :: Distance
      in (not (a <= b && b <= c) || (a <= c))
    prop "Reflexivity" $ \x ->
      let a = x :: Distance
      in a <= a
    prop "Antisymmetry" $ \x y ->
      let a = x :: Distance
          b = y :: Distance
      in (not (a <= b && b <= a) || (a == b))
    prop "x >= y = y <= x" $ \x y ->
      let a = x :: Distance
          b = y :: Distance
      in (a >= b) == (b <= a)
    prop "x < y = x <= y && x /= y" $ \x y ->
      let a = x :: Distance
          b = y :: Distance
      in (a < b) == ((a <= b) && a /= b)
    prop "x > y = y < x" $ \x y ->
      let a = x :: Distance
          b = y :: Distance
      in (a > b) == (b < a)
    prop "x < y = compare x y == LT" $ \x y ->
      let a = x :: Distance
          b = y :: Distance
      in (a < b) == (compare a b == LT)
    prop "x > y = compare x y == GT" $ \x y ->
      let a = x :: Distance
          b = y :: Distance
      in (a > b) == (compare a b == GT)
    prop "x == y = compare x y == EQ" $ \x y ->
      let a = x :: Distance
          b = y :: Distance
      in (a == b) == (compare a b == EQ)
    prop "min x y == if x <= y then x else y = True" $ \x y ->
      let a = x :: Distance
          b = y :: Distance
      in min a b == if a <= b then a else b
    prop "max x y == if x >= y then x else y = True" $ \x y ->
      let a = x :: Distance
          b = y :: Distance
      in max a b == if a >= b then a else b
