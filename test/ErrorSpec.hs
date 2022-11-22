{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module ErrorSpec (spec) where

import           Betitla.Error

import           Witch                 (as, from)

import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
  describe "errorForNothing" $ do
    prop "chicks for free" $ \x ->
      errorForNothing (StriveError "foo") (Just @Integer x) `shouldBe` Right x
    prop "error for nothing" $ \x ->
      errorForNothing (StriveError $ from @String x)
                      (as @(Maybe Integer) Nothing)
        `shouldBe` Left (StriveError $ from @String x)
