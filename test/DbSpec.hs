--{-# LANGUAGE FlexibleContexts      #-}
module DbSpec (spec) where

import           Betitla.Db
import           Betitla.Error
import           Betitla.Lenses
import           Betitla.Striver

import           Control.Lens.Getter    ((^.))
import           Control.Lens.Setter    ((.~))
import           Data.Either            (isLeft, isRight)
import           Data.Time.Clock.System (SystemTime (..))
import           System.Directory       (doesFileExist, removeFile)
import           Test.Hspec
import           Test.Hspec.QuickCheck  (prop)

testDbName :: String
testDbName = "._test.db"

testToken :: AccessToken
testToken = AccessToken "cafebabe" "deadbeef" $ MkSystemTime 10 0

spec :: Spec
spec = do

  describe "test DB must not exist" $
    it "no existance" $ do
      exists <- doesFileExist testDbName
      exists `shouldBe` False

  describe "inserts striver" $
    let insertTest = addStriverToDb testToken 1 >> selectStriverFromDb 1
    in it "succesfully" $ do
      result <- withDb testDbName insertTest
      isRight result `shouldBe` True

  describe "selects striver" $
    let selectTest = selectStriverFromDb 1
    in it "succesfully" $ do
      result <- withDb testDbName selectTest
      case result of
        Left _  -> False `shouldBe` True
        Right x -> accessTokenFromStriver x `shouldBe` testToken

  describe "updates striver" $
    let newToken = access .~ "babecafe" $ testToken
        updateTest = updateStriverInDb newToken 1 >> selectStriverFromDb 1
    in it "succesfully" $ do
      result <- withDb testDbName updateTest
      case result of
        Left _  -> False `shouldBe` True
        Right x -> accessTokenFromStriver x ^. access `shouldBe` "babecafe"

  describe "deletes striver" $
    let deleteTest = deleteStriverFromDb 1 >> selectStriverFromDb 1
    in it "succesfully" $ do
      result <- withDb testDbName deleteTest
      isLeft result `shouldBe` True

  describe "inserts activity" $
    let insertTest = addActivityToDb 1 2 >> selectActivitiesFromDb 1
    in it "succesfully" $ do
      result <- withDb testDbName insertTest
      isRight result `shouldBe` True

  describe "selects activity" $
    let selectTest = selectActivitiesFromDb 1
    in it "succesfully" $ do
      result <- withDb testDbName selectTest
      case result of
        Left _ -> False `shouldBe` True
        Right x -> length x `shouldBe` 1

  describe "selects activities" $
    let selectTest = mapM_ (addActivityToDb 1) [3..101] >> selectActivitiesFromDb 1
    in it "succesfully" $ do
      result <- withDb testDbName selectTest
      case result of
        Left _ -> False `shouldBe` True
        Right x -> length x `shouldBe` 100

  describe "selects activity by activity id" $
    let selectTest = selectActivityFromDb 3
    in it "succesfully" $ do
      result <- withDb testDbName selectTest
      case result of
        Left _ -> False `shouldBe` True
        Right x -> x ^. activityId `shouldBe` 3

  describe "test DB must be obliterated" $
    it "is gone" $ do
      removeFile testDbName
      exists <- doesFileExist testDbName
      exists `shouldBe` False
