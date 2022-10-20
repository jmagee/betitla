{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Betitla.Striver
( ClientSecret (..)
, AthleteId (..)
, ActivityId (..)
, AuthCode (..)
, AppId (..)
, getSecretToken
, getAppId
, getAccessToken
, refreshAccessToken
, getIdByToken
, newUser
, oldUser
, recordActivity
, refreshUser
, removeUser

, getAppInfo
) where

import           Betitla.AccessToken
import           Betitla.Db
import           Betitla.Env
import           Betitla.Error
import           Betitla.Lenses
import           Betitla.StriverIds
import           Betitla.Util

import           Control.Applicative        (liftA3)
import           Control.Lens.Getter        ((^.))
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (Reader, ReaderT, asks)
import           Control.Monad.Trans.Either (EitherT (..), hoistEither,
                                             newEitherT, runEitherT)
import           Data.Bifunctor             (bimap)
import           Data.ByteString.Lazy       (ByteString)
import           Data.Functor               ((<&>))
import qualified Data.Map                   as M (lookup)
import           Data.String.Conversions    (cs)
import           Data.Text                  (Text, append)
import           Data.Time.Clock.System     (SystemTime (..), getSystemTime)
import           Network.HTTP.Client        (Response)
import           Path                       (Abs, File, Path, parseAbsFile,
                                             toFilePath)
import qualified Strive                     as S (id)
import           Strive                     (AthleteSummary, Client, Result,
                                             TokenExchangeResponse, accessToken,
                                             buildClient, exchangeToken,
                                             expiresAt,
                                             getCurrentAthleteSummary,
                                             refreshExchangeToken, refreshToken, deauthorize)

type ReaderIO a b = ReaderT a IO b

newtype ClientSecret = ClientSecret String
                     deriving (Show)

newtype AppId = AppId Integer
              deriving (Show)

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
getDbName = do
  x  <- askToken "Db.name" id
  case x of
    Nothing -> pure Nothing
    Just x' -> pure $ parseAbsFile x'

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

-- | Just Strive's buildClient without the Maybe argument.
buildClient' :: Text -> IO Client
buildClient' = buildClient . Just

-- | Get the athelete ID that corrosponds to the AccessToken.
getIdByToken :: AccessToken -> IO (Either Error AthleteId)
getIdByToken token = do
  client <- buildClient' $ cs $ token ^. access
  getCurrentAthleteSummary client <&>
    bimap propagateStriveError (athleteIdFromInteger . (^. S.id))

-- | Deauthorize the AccessToken
deauthorizeByToken :: MonadIO m => AccessToken -> m (Either Error AccessToken)
deauthorizeByToken token = do
  client <- liftIO (buildClient' $ cs $ token ^. access)
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
    app    <- hoistEither $ errorForNothing StriveError "Could not read AppId" appId
    secret <- hoistEither $ errorForNothing StriveError "Could not read App Secret" secret
    name   <- hoistEither $ errorForNothing DBError "Db.name not specified in configuration file" dbName
    pure $ (app, secret, name)

newUser' :: AuthCode -> ReaderIO Env (Either Error AccessToken)
newUser' auth = getAppInfo >>= \info ->
  liftIO $ runEitherT $ do
    (appId, secret, dbName) <- hoistEither info
    token  <- newEitherT  $ getAccessToken appId secret auth
    aId    <- newEitherT  $ getIdByToken token
    _      <- newEitherT  $ addNewUserToDb dbName token aId
    pure token

-- | Get the access token for a new user and save it in the database.
newUser :: AuthCode -> ReaderIO Env (Either Error AccessToken)
newUser auth = do
  appId  <- getAppId
  secret <- getSecretToken
  dbName <- getDbName
  liftIO $ go appId secret dbName
    where
      go :: Maybe AppId -> Maybe ClientSecret -> Maybe (Path Abs File) -> IO (Either Error AccessToken)
      go appId clientSecret db = runEitherT $ do
        app    <- hoistEither $ errorForNothing StriveError "Could not read AppId" appId
        secret <- hoistEither $ errorForNothing StriveError "Could not read App Secret" clientSecret
        name   <- hoistEither $ errorForNothing DBError "Db.name not specified in configuration file" db
        token  <- newEitherT  $ getAccessToken app secret auth
        aId    <- newEitherT  $ getIdByToken token
        _      <- newEitherT  $ addNewUserToDb name token aId
        pure token

-- | Add a new user to the db.
-- This first checks if the user already exists and returns an error if so.
addNewUserToDb :: Path Abs File -> AccessToken -> AthleteId -> IO (Either Error ())
addNewUserToDb dbName token aId =
  withDb (toFilePath dbName) $
    doesStriverExistInDb aId >>= \case
      False -> Right <$> addStriverToDb token aId
      True  -> pure $ Left $ DBDuplicateError ("User " `append` tshow aId `append` " is not new")

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

--deauthorizeByToken :: AccessToken -> IO (Either Error AccessToken)
--deauthorize :: Client -> IO (Result DeauthorizationResponse)
--deleteStriverFromDb :: MonadBeam Sqlite m => AthleteId -> m ()

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
