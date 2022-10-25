{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Betitla
( rename
, rename'
--, testStrive
--, testStrive'
) where

import           Betitla.AccessToken
import           Betitla.Env
import           Betitla.Error
import           Betitla.Sentence
import           Betitla.Striver
import           Betitla.StriverIds
import           Betitla.Util

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (lift, runReaderT)
import           Control.Monad.Trans.Either (hoistEither, newEitherT,
                                             runEitherT)

import           Data.Bool                  (bool)
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime (..))
import           Data.Time.Calendar         (fromGregorian)
import           Witch                      (from)
--import           Strive

{-testStrive :: IO ()-}
{-testStrive = do-}
  {-let token = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" -- refresh token-}
  {-client <- buildClient (Just token)-}
  {-currentAthlete <- getCurrentAthlete client-}
  {-print (currentAthlete :: Result AthleteDetailed)-}
  {-anotherAthlete <- getAthlete client 41517320-}
  {-print (anotherAthlete  :: Result AthleteSummary)-}


  {-currentActivities <- getCurrentActivities client $ with-}
    {-[ set before (Just (UTCTime (fromGregorian 2022 10 0) 0))-}
    {-, set after (Just (UTCTime (fromGregorian 2022 8 0) 0))-}
    {-, set page 1-}
    {-, set perPage 5-}
    {-]-}
  {-print (currentActivities :: Result [ActivitySummary])-}

{-testStrive' :: IO ()-}
{-testStrive' = do-}
  {-let authorizeUrl = buildAuthorizeUrl 94899 {-1790-} "http://localhost" $ with [ set approvalPrompt False , set readScope True , set readAllScope True, set activityReadAllScope True , set activityWriteScope True] --, set state "..."]-}
  {---print $ (authorizeUrl :: String) ++ "&scope=activity:read"-}
  {-print (authorizeUrl :: String)-}

  {-tokenExchangeResponse <- exchangeToken 94899 "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"-}
  {-print (tokenExchangeResponse :: Result TokenExchangeResponse)-}

rename' :: AthleteId -> ActivityId -> IO (Either Error Text)
rename' athlete activity = getEnvRC >>= \env -> runReaderT (go env) env
  where
    go env = runEitherT $ do
      returner <- newEitherT $ doIKnowYou athlete
      returnerGuard_ returner

      oldToken <- newEitherT $ oldUser athlete
      alive    <- liftIO $ isTokenAlive oldToken
      token    <- alive ? pure oldToken
                        $ newEitherT $ refreshUser oldToken

      isNew    <- newEitherT $ isActivityNew activity
      newGuard_ isNew

      rating   <- newEitherT $ extractActivityRating token activity
      newTitle <- lift $ genSentence SentenceSimple rating
      updatedTitle <- newEitherT $ newActivityTitle token activity athlete (from newTitle)
      pure updatedTitle
    newGuard_ = eitherTGuard_ (BErrorNotNew $ tshow activity)
    returnerGuard_ = eitherTGuard_ (BErrorPurse $ tshow athlete)

rename :: IO String
rename = pure "blah"
