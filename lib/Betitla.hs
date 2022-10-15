{-# LANGUAGE OverloadedStrings #-}

module Betitla
( rename
, testStrive
, testStrive'
) where


import           Data.Time          (NominalDiffTime, UTCTime (..))
import           Data.Time.Calendar (fromGregorian)
import           Strive

testStrive :: IO ()
testStrive = do
  let token = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" -- refresh token
  client <- buildClient (Just token)
  currentAthlete <- getCurrentAthlete client
  print (currentAthlete :: Result AthleteDetailed)
  anotherAthlete <- getAthlete client 41517320
  print (anotherAthlete  :: Result AthleteSummary)


  currentActivities <- getCurrentActivities client $ with
    [ set before (Just (UTCTime (fromGregorian 2022 10 0) 0))
    , set after (Just (UTCTime (fromGregorian 2022 8 0) 0))
    , set page 1
    , set perPage 5
    ]
  print (currentActivities :: Result [ActivitySummary])

testStrive' :: IO ()
testStrive' = do
  let authorizeUrl = buildAuthorizeUrl 94899 {-1790-} "http://localhost" $ with [ set approvalPrompt False , set readScope True , set readAllScope True, set activityReadAllScope True , set activityWriteScope True] --, set state "..."]
  --print $ (authorizeUrl :: String) ++ "&scope=activity:read"
  print (authorizeUrl :: String)

  tokenExchangeResponse <- exchangeToken 94899 "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  print (tokenExchangeResponse :: Result TokenExchangeResponse)

rename :: IO String
rename = pure "blah"
