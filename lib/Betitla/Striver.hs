{-# LANGUAGE OverloadedStrings     #-}

module Betitla.Striver
( ClientSecret (..)
, AuthCode (..)
, AppId (..)
, buildAccessToken
, getSecretToken
, getAppId
, getAccessToken
, refreshAccessToken
, getIdByToken
) where

import           Betitla.AccessToken
import           Betitla.Env
import           Betitla.Error
import           Betitla.Lenses

import           Control.Lens.Getter     ((^.))
import           Control.Monad.Reader    (Reader, ReaderT, asks)
import           Data.ByteString.Lazy    (ByteString)
import           Data.Functor            ((<&>))
import           Data.Int                (Int64)
import qualified Data.Map                as M (lookup)
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import           Data.Time.Clock.System  (SystemTime (..), getSystemTime)
import           Network.HTTP.Client     (Response)
import qualified Strive                  as S (id)
import           Strive                  (AthleteSummary, Client, Result,
                                          TokenExchangeResponse, accessToken,
                                          buildClient, exchangeToken, expiresAt,
                                          getCurrentAthleteSummary,
                                          refreshExchangeToken, refreshToken)
import           Strive.Lenses           (get)

type ReaderIO a b = ReaderT a IO b

newtype ClientSecret = ClientSecret String
                     deriving (Show)

newtype AppId = AppId Integer
              deriving (Show)

newtype AuthCode = AuthCode String
                 deriving (Show)

newtype AthleteId = AthleteId Integer
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

-- | Promote an error from the underlying Strive library into a Betitla Error type.
propagateStriveError :: (Response ByteString, String) -> Error
propagateStriveError (full, msg) = errorFromString StriveError $ show full ++ " ; " ++ msg

-- | Construct an AccessToken from the raw token parts.
buildAccessToken :: Text -> Text -> Integer -> AccessToken
buildAccessToken access refresh expires =
  AccessToken (cs access)
              (cs refresh)
              (MkSystemTime (fromIntegral expires) 0)

-- | Using a 1-time authorization code, request the AccessToken from the remote.
getAccessToken :: AppId -> ClientSecret -> AuthCode -> IO (Either Error AccessToken)
getAccessToken (AppId appId) (ClientSecret secret) (AuthCode code) =
  exchangeToken appId secret code <&>
    either (Left . propagateStriveError)
           (\ok -> Right $ buildAccessToken (get accessToken ok)
                                            (get refreshToken ok)
                                            (get expiresAt ok))

-- | Given an existing AccessToken, request an updated (refreshed) token.
-- If the token is not expired the remote API will return the existing token.
-- Therefore, unnecessary calls to this are safe from a correctness point of view,
-- although naturally count against the API call limit.  Use isTokenExpired or
-- isTokenAlive to guard against such unnecessary calls.
refreshAccessToken :: AppId -> ClientSecret -> AccessToken -> IO (Either Error AccessToken)
refreshAccessToken (AppId appId) (ClientSecret secret) (AccessToken _ refresh _) =
  refreshExchangeToken appId secret refresh <&>
    either (Left . propagateStriveError)
           (\ok -> Right $ buildAccessToken (ok ^. accessToken)
                                            (ok ^. refreshToken)
                                            (ok ^. expiresAt))

-- | Just Strive's buildClient without the Maybe argument.
buildClient' :: Text -> IO Client
buildClient' = buildClient . Just

-- | Get the athelete ID that corrosponds to the AccessToken.
getIdByToken :: AccessToken -> IO (Either Error AthleteId)
getIdByToken token = do
  client <- buildClient' $ cs $ token ^. access
  getCurrentAthleteSummary client <&>
    either (Left . propagateStriveError)
           (\ok -> Right $ AthleteId $ ok ^. S.id)

{--- | Get the access token for a new user and save it in the database.-}
{-newUser :: AuthCode -> ReaderIO Env (Either Error AccessToken)-}
{-newUser auth = do-}
  {-appId  <- getAppId-}
  {-secret <- getSecretToken-}
  {-token  <- getAccessToken appId secret auth-}
  {-case token of-}
    {-Left err -> Left err-}
    {-Right t  -> do-}
      {-aid <- getIdByToken t-}
      {-case aid of-}
        {-Left err -> Left err-}
        {-Right realId -> -- ... Right t-}

