module RenamerSpec (spec) where

import           Betitla.Distance
import           Betitla.Speed
import           Betitla.Time

import           Test.Hspec
import           Test.Hspec.QuickCheck  (prop)

reflexivity :: Eq a => a -> Bool
reflexivity x = x == x

symmetry :: Eq a => a -> a -> Bool
symmetry x y = (x == y) == (y == x)

transitivityEq :: Eq a => a -> a -> a -> Bool
transitivityEq x y z = not (x == y && y == z) || (x == z)

substitutivity :: Eq a => a -> a -> Bool
substitutivity x y = if x == y then f x == f y else f x /= f y
  where f = id

negation :: Eq a => a -> a -> Bool
negation x y = (x /= y) == not (x == y)

transitivityOrd :: Ord a => a -> a -> a -> Bool
transitivityOrd x y z = not (x <= y && y <= z) || (x <= z)

reflexivityOrd :: Ord a => a -> Bool
reflexivityOrd x = x <= x

antisymmetry :: Ord a => a -> a -> Bool
antisymmetry x y = not (x <= y && y <= x) || (x == y)

ordPropA :: Ord a => a -> a -> Bool
ordPropA x y = (x >= y) == (y <= x)

ordPropB :: Ord a => a -> a -> Bool
ordPropB x y = (x < y) == ((x <= y) && x /= y)

ordPropC :: Ord a => a -> a -> Bool
ordPropC x y = (x > y) == (y < x)

ordPropD :: Ord a => a -> a -> Bool
ordPropD x y = (x < y) == (compare x y == LT)

ordPropE :: Ord a => a -> a -> Bool
ordPropE x y = (x > y) == (compare x y == GT)

ordPropF :: Ord a => a -> a -> Bool
ordPropF x y = (x == y) == (compare x y == EQ)

ordPropG :: Ord a => a -> a -> Bool
ordPropG x y = min x y == if x <= y then x else y

ordPropH :: Ord a => a -> a -> Bool
ordPropH x y = max x y == if x >= y then x else y

spec :: Spec
spec = do

  describe "meters/kilometer conversions" $ do
    prop "a kilometer is one thousand meters" $ \x ->
      let check = x `div` 1000
      in toKm (Meters x) == Kilometers check
    prop "a meter is one thousandth of a kilometer" $ \x ->
      let check = x * 1000
      in toMeters (Kilometers x) == Meters check

  describe "Eq Distance" $ do
    prop "Reflexivity" (reflexivity :: Distance -> Bool)
    prop "Symmetry" (symmetry :: Distance -> Distance -> Bool)
    prop "Transitivity" (transitivityEq :: Distance -> Distance -> Distance -> Bool)
    prop "Substitutivity" (substitutivity :: Distance -> Distance -> Bool)
    prop "Negation" (negation :: Distance -> Distance -> Bool)

  describe "Ord Distance" $ do
    prop "Transitivity" (transitivityOrd :: Distance -> Distance -> Distance -> Bool)
    prop "Reflexivity" (reflexivityOrd :: Distance -> Bool)
    prop "Antisymmetry" (antisymmetry :: Distance -> Distance -> Bool)
    prop "x >= y = y <= x" (ordPropA :: Distance -> Distance -> Bool)
    prop "x < y = x <= y && x /= y" (ordPropB :: Distance -> Distance -> Bool)
    prop "x > y = y < x" (ordPropC :: Distance -> Distance -> Bool)
    prop "x < y = compare x y == LT" (ordPropD :: Distance -> Distance -> Bool)
    prop "x > y = compare x y == GT" (ordPropE :: Distance -> Distance -> Bool)
    prop "x == y = compare x y == EQ" (ordPropF :: Distance -> Distance -> Bool)
    prop "min x y == if x <= y then x else y = True" (ordPropG :: Distance -> Distance -> Bool)
    prop "max x y == if x >= y then x else y = True" (ordPropH :: Distance -> Distance -> Bool)

  describe "Eq Duration" $ do
    prop "Reflexivity" (reflexivity :: Duration -> Bool)
    prop "Symmetry" (symmetry :: Duration -> Duration -> Bool)
    prop "Transitivity" (transitivityEq :: Duration -> Duration -> Duration -> Bool)
    prop "Substitutivity" (substitutivity :: Duration -> Duration -> Bool)
    prop "Negation" (negation :: Duration -> Duration -> Bool)

  describe "Ord Duration" $ do
    prop "Transitivity" (transitivityOrd :: Duration -> Duration -> Duration -> Bool)
    prop "Reflexivity" (reflexivityOrd :: Duration -> Bool)
    prop "Antisymmetry" (antisymmetry :: Duration -> Duration -> Bool)
    prop "x >= y = y <= x" (ordPropA :: Duration -> Duration -> Bool)
    prop "x < y = x <= y && x /= y" (ordPropB :: Duration -> Duration -> Bool)
    prop "x > y = y < x" (ordPropC :: Duration -> Duration -> Bool)
    prop "x < y = compare x y == LT" (ordPropD :: Duration -> Duration -> Bool)
    prop "x > y = compare x y == GT" (ordPropE :: Duration -> Duration -> Bool)
    prop "x == y = compare x y == EQ" (ordPropF :: Duration -> Duration -> Bool)
    prop "min x y == if x <= y then x else y = True" (ordPropG :: Duration -> Duration -> Bool)
    prop "max x y == if x >= y then x else y = True" (ordPropH :: Duration -> Duration -> Bool)

  describe "Eq Elevation" $ do
    prop "Reflexivity" (reflexivity :: Elevation -> Bool)
    prop "Symmetry" (symmetry :: Elevation -> Elevation -> Bool)
    prop "Transitivity" (transitivityEq :: Elevation -> Elevation -> Elevation -> Bool)
    prop "Substitutivity" (substitutivity :: Elevation -> Elevation -> Bool)
    prop "Negation" (negation :: Elevation -> Elevation -> Bool)

  describe "Ord Elevation" $ do
    prop "Transitivity" (transitivityOrd :: Elevation -> Elevation -> Elevation -> Bool)
    prop "Reflexivity" (reflexivityOrd :: Elevation -> Bool)
    prop "Antisymmetry" (antisymmetry :: Elevation -> Elevation -> Bool)
    prop "x >= y = y <= x" (ordPropA :: Elevation -> Elevation -> Bool)
    prop "x < y = x <= y && x /= y" (ordPropB :: Elevation -> Elevation -> Bool)
    prop "x > y = y < x" (ordPropC :: Elevation -> Elevation -> Bool)
    prop "x < y = compare x y == LT" (ordPropD :: Elevation -> Elevation -> Bool)
    prop "x > y = compare x y == GT" (ordPropE :: Elevation -> Elevation -> Bool)
    prop "x == y = compare x y == EQ" (ordPropF :: Elevation -> Elevation -> Bool)
    prop "min x y == if x <= y then x else y = True" (ordPropG :: Elevation -> Elevation -> Bool)
    prop "max x y == if x >= y then x else y = True" (ordPropH :: Elevation -> Elevation -> Bool)

  describe "Eq Speed" $ do
    prop "Reflexivity" (reflexivity :: Speed -> Bool)
    prop "Symmetry" (symmetry :: Speed -> Speed -> Bool)
    prop "Transitivity" (transitivityEq :: Speed -> Speed -> Speed -> Bool)
    prop "Substitutivity" (substitutivity :: Speed -> Speed -> Bool)
    prop "Negation" (negation :: Speed -> Speed -> Bool)

  describe "Ord Speed" $ do
    prop "Transitivity" (transitivityOrd :: Speed -> Speed -> Speed -> Bool)
    prop "Reflexivity" (reflexivityOrd :: Speed -> Bool)
    prop "Antisymmetry" (antisymmetry :: Speed -> Speed -> Bool)
    prop "x >= y = y <= x" (ordPropA :: Speed -> Speed -> Bool)
    prop "x < y = x <= y && x /= y" (ordPropB :: Speed -> Speed -> Bool)
    prop "x > y = y < x" (ordPropC :: Speed -> Speed -> Bool)
    prop "x < y = compare x y == LT" (ordPropD :: Speed -> Speed -> Bool)
    prop "x > y = compare x y == GT" (ordPropE :: Speed -> Speed -> Bool)
    prop "x == y = compare x y == EQ" (ordPropF :: Speed -> Speed -> Bool)
    prop "min x y == if x <= y then x else y = True" (ordPropG :: Speed -> Speed -> Bool)
    prop "max x y == if x >= y then x else y = True" (ordPropH :: Speed -> Speed -> Bool)

  describe "Eq DistanceRating" $ do
    prop "Reflexivity" (reflexivity :: DistanceRating -> Bool)
    prop "Symmetry" (symmetry :: DistanceRating -> DistanceRating -> Bool)
    prop "Transitivity" (transitivityEq :: DistanceRating -> DistanceRating -> DistanceRating -> Bool)
    prop "Substitutivity" (substitutivity :: DistanceRating -> DistanceRating -> Bool)
    prop "Negation" (negation :: DistanceRating -> DistanceRating -> Bool)

  describe "Eq ElevationRating" $ do
    prop "Reflexivity" (reflexivity :: ElevationRating -> Bool)
    prop "Symmetry" (symmetry :: ElevationRating -> ElevationRating -> Bool)
    prop "Transitivity" (transitivityEq :: ElevationRating -> ElevationRating -> ElevationRating -> Bool)
    prop "Substitutivity" (substitutivity :: ElevationRating -> ElevationRating -> Bool)
    prop "Negation" (negation :: ElevationRating -> ElevationRating -> Bool)

  describe "Eq PhaseOfDay" $ do
    prop "Reflexivity" (reflexivity :: PhaseOfDay -> Bool)
    prop "Symmetry" (symmetry :: PhaseOfDay -> PhaseOfDay -> Bool)
    prop "Transitivity" (transitivityEq :: PhaseOfDay -> PhaseOfDay -> PhaseOfDay -> Bool)
    prop "Substitutivity" (substitutivity :: PhaseOfDay -> PhaseOfDay -> Bool)
    prop "Negation" (negation :: PhaseOfDay -> PhaseOfDay -> Bool)

  describe "Eq DurationRating" $ do
    prop "Reflexivity" (reflexivity :: DurationRating -> Bool)
    prop "Symmetry" (symmetry :: DurationRating -> DurationRating -> Bool)
    prop "Transitivity" (transitivityEq :: DurationRating -> DurationRating -> DurationRating -> Bool)
    prop "Substitutivity" (substitutivity :: DurationRating -> DurationRating -> Bool)
    prop "Negation" (negation :: DurationRating -> DurationRating -> Bool)
