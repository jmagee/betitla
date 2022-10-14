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

  describe "inserts stuff" $
    let insertTest = addStriverToDb testToken 1 >> selectStriverFromDb 1
    in it "succesfully" $ do
      result <- withDb testDbName insertTest
      isRight result `shouldBe` True

  describe "selects stuff" $
    let selectTest = selectStriverFromDb 1
    in it "succesfully" $ do
      result <- withDb testDbName selectTest
      case result of
        Left _  -> False `shouldBe` True
        Right x -> accessTokenFromStriver x `shouldBe` testToken

  describe "updates stuff" $
    let newToken = access .~ "babecafe" $ testToken
        updateTest = updateStriverInDb newToken 1 >> selectStriverFromDb 1
    in it "succesfully" $ do
      result <- withDb testDbName updateTest
      case result of
        Left _  -> False `shouldBe` True
        Right x -> accessTokenFromStriver x ^. access `shouldBe` "babecafe"

  describe "deletes stuff" $
    let deleteTest = deleteStriverFromDb 1 >> selectStriverFromDb 1
    in it "succesfully" $ do
      result <- withDb testDbName deleteTest
      isLeft result `shouldBe` True

  describe "test DB must be obliterated" $
    it "is gone" $ do
      removeFile testDbName
      exists <- doesFileExist testDbName
      exists `shouldBe` False

        --(_striverAccessToken striver) `shouldBe` (_accessToken testToken)

          {-, _striverAccessToken   :: Columnar f Text-}
          {-, _striverRefreshToken  :: Columnar f Text-}
          {-, _striverExpiration    :: Columnar f Int64-}
--addStriverToDb :: MonadBeam Sqlite m => AccessToken -> Integer ->  m ()
--selectStriverFromDb :: MonadBeam Sqlite m => Integer -> m (Either Error Striver)
