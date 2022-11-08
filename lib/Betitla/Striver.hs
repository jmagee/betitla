{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Betitla.Striver
( ClientSecret (..)
, AthleteId (..)
, ActivityId (..)
, AuthCode (..)
, AppId (..)
, getAuthUrl
, getSecretToken
, getAppId
, getAccessToken
, getSlogan
, refreshAccessToken
, getIdByToken
, newUser
, oldUser
, recordActivity
, refreshUser
, removeUser
, dropUser
, isActivityNew
, newActivityTitle
, getAppInfo
, extractActivityRating
, doIKnowYou
, hasRequiredScope
) where

import           Betitla.AccessToken
import           Betitla.ActivityRating
import           Betitla.Db
import           Betitla.Distance
import           Betitla.Env
import           Betitla.Error
import           Betitla.Lenses
import           Betitla.Speed
import           Betitla.Sport
import           Betitla.StriverIds
import           Betitla.Time
import           Betitla.Util

import           Control.Applicative        (liftA3)
import           Control.Lens.Getter        ((^.))
import           Control.Lens.Setter        (set, (.~))
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (ReaderT, asks, lift)
import           Control.Monad.Trans.Either (hoistEither, newEitherT,
                                             runEitherT, hoistMaybe)
import           Data.Bifunctor             (bimap)
import           Data.ByteString.Lazy       (ByteString)
import           Data.Functor               ((<&>))
import qualified Data.Map              as M (lookup)
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text, append, isInfixOf)
import           Data.Time.LocalTime        (utcToLocalTime)
import           Network.HTTP.Client        (Response)
import           Path                       (Abs, File, Path, parseAbsFile,
                                             toFilePath)
import qualified Strive                as S (ActivityType (..),
                                             averageSpeed, description,
                                             distance, id, movingTime,
                                             name, startDate, timezone,
                                             totalElevationGain, type_)
import           Strive                     (ActivityDetailed, AthleteSummary,
                                             Client, Result,
                                             TokenExchangeResponse, accessToken,
                                             allEfforts, buildClient,
                                             deauthorize, exchangeToken,
                                             expiresAt, getActivityDetailed,
                                             getCurrentAthleteSummary,
                                             refreshExchangeToken, refreshToken,
                                             updateActivity, with, buildAuthorizeUrl,
                                             approvalPrompt, readAllScope, activityReadAllScope,
                                             activityWriteScope, readScope)
import           Witch                      (From, from, unsafeFrom)

type ReaderIO a b = ReaderT a IO b

newtype ClientSecret = ClientSecret String
                     deriving (Show)

newtype AppId = AppId Integer
              deriving (Show)

instance From AppId Integer where
  from (AppId x) = x

newtype AuthCode = AuthCode String
                 deriving (Show)

-- | Helper to fetch and construct a token-like thing from the Env.
askToken :: String -> (String -> a) -> ReaderIO Env (Maybe a)
askToken term cons = asks $ fmap cons . M.lookup term

-- | Helper to fetch the secret token from the reader Env.
getSecretToken :: ReaderIO Env (Maybe ClientSecret)
getSecretToken = askToken "Client.secret" ClientSecret

-- | Helper to fetch the appId from the Reader Env.
getAppId :: ReaderIO Env (Maybe AppId)
getAppId = askToken "App.id" makeAppId
  where
    makeAppId string = AppId (read string :: Integer)

-- | Helper to fetch the dbName from the Reader Env.
getDbName :: ReaderIO Env (Maybe (Path Abs File))
getDbName = askToken "Db.name" id >>= \case
  Nothing -> pure Nothing
  Just x' -> pure $ parseAbsFile x'

-- | Helper to get the app slogan.
getSlogan :: ReaderIO Env (Maybe Text)
getSlogan = askToken "App.slogan" from

-- | Promote an error from the underlying Strive library into a Betitla Error type.
propagateStriveError :: (Response ByteString, String) -> Error
propagateStriveError (full, msg) = errorFromString StriveError $ show full ++ " ; " ++ msg

-- | Using a 1-time authorization code, request the AccessToken from the remote.
getAccessToken :: AppId -> ClientSecret -> AuthCode -> IO (Either Error AccessToken)
getAccessToken (AppId appId) (ClientSecret secret) (AuthCode code) =
  exchangeToken appId secret code <&>
    bimap propagateStriveError
          (liftA3 buildAccessToken
            (^. accessToken)
            (^. refreshToken)
            (^. expiresAt))

-- | Given an existing AccessToken, request an updated (refreshed) token.
-- If the token is not expired the remote API will return the existing token.
-- Therefore, unnecessary calls to this are safe from a correctness point of view,
-- although naturally count against the API call limit.  Use isTokenExpired or
-- isTokenAlive to guard against such unnecessary calls.
refreshAccessToken :: AppId -> ClientSecret -> AccessToken -> IO (Either Error AccessToken)
refreshAccessToken (AppId appId) (ClientSecret secret) token =
  refreshExchangeToken appId secret (token ^. refresh) <&>
    bimap propagateStriveError
          (liftA3 buildAccessToken
            (^. accessToken)
            (^. refreshToken)
            (^. expiresAt))

getAuthUrl :: ReaderIO Env (Either Error String)
getAuthUrl = do
  appId <- getAppId
  host  <- askToken "Host.url" id
  pure $ buildAuthorizeUrl <$> fmap from appId
                           <*> host
                           <*> Just (with [ set approvalPrompt False
                                          , set readScope True
                                          , set readAllScope True
                                          , set activityReadAllScope True
                                          , set activityWriteScope True
                                          ])
  <&> errorForNothing' (StriveError "Missing AppId or Host url")

-- | Get the description for an activity.
getActivityDescription :: MonadIO m => AccessToken -> ActivityId -> m (Either Error Text)
getActivityDescription token act = do
  client  <- liftIO $ buildClient' $ from $ token ^. access
  liftIO $ getActivityDetailed client (from act) (with [set allEfforts True]) <&>
    bimap propagateStriveError (fromMaybe "" . (^. S.description))

-- | Update an activity with a new title on Strive.
-- Title and description are updated in one request.  The main reason for this
-- is that while updating the title alone works, updating only the description hits
-- an internal serever error on the remote.
-- A happy side effect of combining them is reducing the number of requests I suppose.
updateActivityTitleAndDesc :: MonadIO m => AccessToken -> ActivityId -> Text -> Text -> m (Either Error Text)
updateActivityTitleAndDesc token act newTitle newDesc = do
  client <- liftIO $ buildClient' $ from $ token ^. access
  liftIO $ updateActivity client (from act) (with [ set S.name (Just $ from newTitle)
                                                  , set S.description (Just $ from newDesc)
                                                  ]) <&>
    bimap propagateStriveError (^. S.name)

-- | Rename the activity title and record it in the database
newActivityTitle :: AccessToken
                 -> ActivityId
                 -> AthleteId
                 -> Text
                 -> ReaderIO Env (Either Error Text)
newActivityTitle token act athlete newTitle = getSlogan >>= \slogan ->
  runEitherT $ do
    isNew        <- newEitherT $ isActivityNew act
    newGuard_ isNew
    oldDesc      <- newEitherT $ getActivityDescription token act
    updatedTitle <- newEitherT $
                      updateActivityTitleAndDesc token act newTitle $
                        newDesc oldDesc slogan
    _            <- newEitherT $ recordActivity athlete act
    pure updatedTitle
  where
    newGuard_ = eitherTGuard_ (BErrorNotNew $ tshow act)
    newDesc old Nothing = old
    newDesc old (Just new) = old `append` "\n\n" `append` new

-- | Just Strive's buildClient without the Maybe argument.
buildClient' :: Text -> IO Client
buildClient' = buildClient . Just

-- | Get the athelete ID that corrosponds to the AccessToken.
getIdByToken :: AccessToken -> IO (Either Error AthleteId)
getIdByToken token = do
  client <- buildClient' $ from $ token ^. access
  getCurrentAthleteSummary client <&>
    bimap propagateStriveError (athleteIdFromInteger . (^. S.id))

-- | Deauthorize the AccessToken
deauthorizeByToken :: MonadIO m => AccessToken -> m (Either Error AccessToken)
deauthorizeByToken token = do
  client <- liftIO (buildClient' $ from $ token ^. access)
  liftIO $ deauthorize client <&>
    bimap propagateStriveError
          (liftA3 buildAccessToken
            (^. accessToken)
            (const "")
            (const 0))

getAppInfo :: ReaderIO Env (Either Error (AppId, ClientSecret, Path Abs File))
getAppInfo = do
  appId  <- getAppId
  secret <- getSecretToken
  dbName <- getDbName
  liftIO $ runEitherT $ do
    appId'  <- hoistEither $ errorForNothing StriveError "Could not read AppId" appId
    secret' <- hoistEither $ errorForNothing StriveError "Could not read App Secret" secret
    dbName' <- hoistEither $ errorForNothing DBError "Db.name not specified in configuration file" dbName
    pure (appId', secret', dbName')

-- | Get the access token for a new user and save it in the database.
newUser :: AuthCode -> ReaderIO Env (Either Error AthleteId)
newUser auth = getAppInfo >>= \info ->
  liftIO $ runEitherT $ do
    (appId, secret, dbName) <- hoistEither info
    token  <- newEitherT  $ getAccessToken appId secret auth
    aId    <- newEitherT  $ getIdByToken token
    _      <- newEitherT  $ addNewUserToDb dbName token aId
    pure aId

-- | Add a new user to the db.
-- This first checks if the user already exists and returns an error if so.
addNewUserToDb :: Path Abs File -> AccessToken -> AthleteId -> IO (Either Error ())
addNewUserToDb dbName token aId =
  withDb (toFilePath dbName) $
    doesStriverExistInDb aId >>= \case
      False -> Right <$> addStriverToDb token aId
      True  -> pure $ Left $ DBDuplicateError ("User " `append` tshow aId `append` " is not new")

-- | Determine if the user is new or returning.
doIKnowYou :: AthleteId -> ReaderIO Env (Either Error Bool)
doIKnowYou you = getDbName >>= \db -> liftIO $ runEitherT $ do
  name <- hoistEither $ errorForNothing DBError "Db.name not specified in confgiruation file" db
  newEitherT $ withDb (toFilePath name)
                      (Right <$> doesStriverExistInDb you)

-- | Get the access token for a returning user
oldUser :: AthleteId -> ReaderIO Env (Either Error AccessToken)
oldUser aId = getDbName >>= liftIO . go
    where
      go :: Maybe (Path Abs File) -> IO (Either Error AccessToken)
      go db = runEitherT $ do
        name    <- hoistEither $ errorForNothing DBError "Db.name not specified in configuration file" db
        striver <- newEitherT (withDb (toFilePath name) $ selectStriverFromDb aId)
        pure $ accessTokenFromStriver striver

-- | Record an activity (meaning it has been processed) for a user.
recordActivity :: AthleteId -> ActivityId -> ReaderIO Env (Either Error ())
recordActivity aId act =
  getDbName >>= liftIO . go
    where
      go :: Maybe (Path Abs File) -> IO (Either Error ())
      go db = runEitherT $ do
        name    <- hoistEither $ errorForNothing DBError "Db.name not specified in configuration file" db
        newEitherT $ withDb (toFilePath name) $ do
          userExists <- doesStriverExistInDb aId
          actExists  <- doesActivityExistInDb act
          case (userExists, actExists) of
            (False, _)    -> pure (Left (DBNotFoundError "User doesn't exist"))
            (True, True)  -> pure (Left (DBDuplicateError "Activity already exists"))
            (True, False) -> Right <$> addActivityToDb aId act

-- | Determine if an activity is new (i.e. hasn't been seen and processed).
isActivityNew :: ActivityId -> ReaderIO Env (Either Error Bool)
isActivityNew act = getDbName >>= \db -> liftIO $ runEitherT $ do
  name <- hoistEither $ errorForNothing DBError "Db.name not specified in configuration file" db
  newEitherT $ withDb (toFilePath name)
                      (Right . not <$> doesActivityExistInDb act)

-- | Remove a user from the records
removeUser :: AthleteId -> ReaderIO Env (Either Error AccessToken)
removeUser athlete =
  getDbName >>= \case
    Nothing   -> pure $ Left (DBError "Db.name not specified in configuration file")
    Just name -> liftIO $ go name
  where
   go name = withDb (toFilePath name) $ runEitherT $ do
      striver   <- newEitherT $ selectStriverFromDb athlete
      deadToken <- newEitherT $ deauthorizeByToken (accessTokenFromStriver striver)
      _         <- newEitherT $ Right <$> deleteStriverFromDb athlete
      pure deadToken

-- | Drop a user from the records
-- This is identical to removeUser, except it doesn't attempt to deauthenticate
-- from the remote.  This should be used when responding to a deauth webhook event.
dropUser :: AthleteId -> ReaderIO Env (Either Error ())
dropUser athlete =
  getDbName >>= \case
    Nothing   -> pure $ Left (DBError "Db.name not specified in configuration file")
    Just name -> liftIO $ go name
  where
   go name = withDb (toFilePath name) $ runEitherT $ do
      striver   <- newEitherT $ selectStriverFromDb athlete
      _         <- newEitherT $ Right <$> deleteStriverFromDb athlete
      pure ()

-- | Refresh the user's AccessToken
refreshUser :: AccessToken -> ReaderIO Env (Either Error AccessToken)
refreshUser oldToken = getAppInfo >>= \info ->
  liftIO $ runEitherT $ do
    (appId, secret, dbName) <- hoistEither info
    newToken <- newEitherT $ refreshAccessToken appId secret oldToken
    aId      <- newEitherT $ getIdByToken newToken
    _        <- newEitherT $ updateDb newToken aId dbName
    pure newToken
  where
    updateDb newToken aId dbName =
      withDb (toFilePath dbName) $ doesStriverExistInDb aId >>=
        \case
          False -> pure $ Left $ DBNotFoundError "User doesn't exist"
          True  -> Right <$> updateStriverInDb newToken aId

{----- | Get the description for an activity.-}
{-getActivityDescription :: MonadIO m => AccessToken -> ActivityId -> m (Either Error Text)-}
{-getActivityDescription token act = do-}
  {-client  <- liftIO $ buildClient' $ from $ token ^. access-}
  {-liftIO $ getActivityDetailed client (from act) (with [set allEfforts True]) <&>-}
    {-bimap propagateStriveError (fromMaybe "" . (^. S.description))-}

-- | Extract ActivityRating from Strive
extractActivityRating :: MonadIO m => AccessToken -> ActivityId -> m (Either Error ActivityRating)
extractActivityRating token act = do
  client   <- liftIO $ buildClient' $ from $ token ^. access
  liftIO $ getActivityDetailed client (from act) (with [set allEfforts True]) <&>
      bimap propagateStriveError makeActivity
  where
    makeActivity :: ActivityDetailed -> ActivityRating
    makeActivity detail =
      let s     = striveTypeToSport $ detail ^. S.type_
          dist  = Meters $ floor $ detail ^. S.distance
          dur   = Seconds $ unsafeFrom $ detail ^. S.movingTime
          ele   = MetersGained $ floor $ detail ^. S.totalElevationGain
          speed = MetersPerSec $ realToFrac $ detail ^. S.averageSpeed
          start = detail ^. S.startDate -- [Note startDateLocal]
          zone  = textToTimeZone $ from (detail ^. S.timezone)
      in ActivityRating
          s
          (distanceToRating s dist)
          (durationToRating s dur)
          (elevationToRating s ele dist)
          (speedToRating s speed)
          (localTimeToPOD (utcToLocalTime zone start))

-- [Note startDateLocal]
-- The way the remove API works is a bit suprising.  startDate is the UTC start date+time.
-- startDateLocal is the local time start date+time.  However, startDateLocal is returned
-- as a UTC time as if it was a local time.  This presents two choices on how to handle thec:
--   - Use the real UTC time (startDate) with the timezone info to calculate localtime ourselves.
--   - Use startDateLocal and convert it to LocalTime using a 0-offset timezone.
--
-- Currently the former is in-use.  But may want to re-consider the latter.
-- The timezone info returned by the remote is a little sus - afaict it doesn't factor in DST.
-- For example, I get (GMT-08:00) America/Los_Angeles despite being in DST, so I should expect
-- (GMT-07:00) America/Los_Angeles.

-- | Convert a strive ActivityType to our Sport.
-- This is a bit silly since they are nearly identical.
-- Might be better to just use a type aliases.
striveTypeToSport :: S.ActivityType -> Sport
striveTypeToSport = \case
  S.AlpineSki
           -> AlpineSki
  S.BackcountrySki
           -> BackcountrySki
  S.Canoeing
           -> Canoeing
  S.CrossCountrySkiing
           -> BackcountrySki
  S.Crossfit
           -> Crossfit
  S.Elliptical
           -> Elliptical
  S.Hike
           -> Hike
  S.IceSkate
           -> IceSkate
  S.InlineSkate
           -> InlineSkate
  S.Kayaking
           -> Kayaking
  S.KiteSurf
           -> Kitesurf
  S.NordicSki
           -> NordicSki
  S.Ride
           -> Ride
  S.RockClimbing
           -> RockClimbing
  S.RollerSki
           -> RollerSki
  S.Rowing
           -> Rowing
  S.Run
           -> Run
  S.Snowboard
           -> Snowboard
  S.Snowshoe
           -> Snowshoe
  S.StairStepper
           -> StairStepper
  S.StandUpPaddling
           -> StandUpPaddling
  S.Surfing
           -> Surfing
  S.Swim
           -> Swim
  S.VirtualRide
           -> VirtualRide
  S.Walk
           -> Walk
  S.WeightTraining
           -> WeightTraining
  S.Windsurf
           -> Windsurf
  S.Workout
           -> Workout
  S.Yoga
           -> Yoga
           {-| EBikeRide-}
           {-| Golf-}
           {-| Handcycle-}
           {-| Sail-}
           {-| Skateboard-}
           {-| Soccer-}
           {-| Velomobile-}
           {-| VirtualRun-}
           {-| Wheelchair-}

-- | Check whether the provided text containts the necessary activity scopes.
hasRequiredScope :: Text -> Bool
hasRequiredScope text = isInfixOf "activity:read" text &&
                        isInfixOf "activity:write" text
