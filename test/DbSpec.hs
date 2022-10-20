{-# LANGUAGE FlexibleContexts      #-}
module DbSpec (spec) where

import           Betitla.AccessToken
import           Betitla.Db
import           Betitla.Error
import           Betitla.Lenses
import           Betitla.Striver
import           Betitla.StriverIds

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

  describe "no striver" $
    let existanceTest = doesStriverExistInDb $ AthleteId 1
    in it "succesfully" $ do
      result <- withDb testDbName existanceTest
      result `shouldBe` False

  describe "inserts striver" $
    let insertTest = addStriverToDb testToken (AthleteId 1) >> selectStriverFromDb (AthleteId 1)
    in it "succesfully" $ do
      result <- withDb testDbName insertTest
      isRight result `shouldBe` True

  describe "yes striver" $
    let existanceTest = doesStriverExistInDb $ AthleteId 1
    in it "succesfully" $ do
      result <- withDb testDbName existanceTest
      result `shouldBe` True

  describe "selects striver" $
    let selectTest = selectStriverFromDb $ AthleteId 1
    in it "succesfully" $ do
      result <- withDb testDbName selectTest
      case result of
        Left _  -> False `shouldBe` True
        Right x -> accessTokenFromStriver x `shouldBe` testToken

  describe "updates striver" $
    let newToken = access .~ "babecafe" $ testToken
        updateTest = updateStriverInDb newToken (AthleteId 1) >> selectStriverFromDb (AthleteId 1)
    in it "succesfully" $ do
      result <- withDb testDbName updateTest
      case result of
        Left _  -> False `shouldBe` True
        Right x -> accessTokenFromStriver x ^. access `shouldBe` "babecafe"

  describe "deletes striver" $
    let deleteTest = deleteStriverFromDb (AthleteId 1) >> selectStriverFromDb (AthleteId 1)
    in it "succesfully" $ do
      result <- withDb testDbName deleteTest
      isLeft result `shouldBe` True

  describe "no activity" $
    let existanceTest = doesActivityExistInDb (ActivityId 2)
    in it "succesfully" $ do
      result <- withDb testDbName existanceTest
      result `shouldBe` False

  describe "inserts activity" $
    let insertTest = addActivityToDb (AthleteId 1) (ActivityId 2) >> selectActivitiesFromDb (AthleteId 1)
    in it "succesfully" $ do
      result <- withDb testDbName insertTest
      isRight result `shouldBe` True

  describe "yes activity" $
    let existanceTest = doesActivityExistInDb (ActivityId 2)
    in it "succesfully" $ do
      result <- withDb testDbName existanceTest
      result `shouldBe` True

  describe "selects activity" $
    let selectTest = selectActivitiesFromDb (AthleteId 1)
    in it "succesfully" $ do
      result <- withDb testDbName selectTest
      case result of
        Left _ -> False `shouldBe` True
        Right x -> length x `shouldBe` 1

  describe "selects activities" $
    let addAct x = addActivityToDb (AthleteId 1) (ActivityId x)
        selectTest = mapM_ addAct [3..101] >> selectActivitiesFromDb (AthleteId 1)
    in it "succesfully" $ do
      result <- withDb testDbName selectTest
      case result of
        Left _ -> False `shouldBe` True
        Right x -> length x `shouldBe` 100

  describe "selects activity by activity id" $
    let selectTest = selectActivityFromDb (ActivityId 3)
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
