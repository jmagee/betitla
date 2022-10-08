{-# LANGUAGE OverloadedStrings     #-}

module Betitla.Striver
( ClientSecret (..)
, AuthCode (..)
, AppId (..)
, AccessToken (..)
, getSecretToken
, getAppId
, getAccessToken
, refreshAccessToken
) where

import           Betitla.Env
import           Betitla.Error

import           Control.Monad.Reader    (Reader, ReaderT, asks)
import           Data.ByteString.Lazy    (ByteString)
import           Data.Functor            ((<&>))
import           Data.Int                (Int64)
import qualified Data.Map                as M (lookup)
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import           Data.Time.Clock.System  (SystemTime (..), getSystemTime)
import           Network.HTTP.Client     (Response)
import           Strive                  (Result, TokenExchangeResponse,
                                          accessToken, exchangeToken, expiresAt,
                                          refreshExchangeToken, refreshToken)
import           Strive.Lenses           (get)

type ReaderIO a b = ReaderT a IO b

newtype ClientSecret = ClientSecret String
                     deriving (Show)

newtype AppId = AppId Integer
              deriving (Show)

data AccessToken =
  AccessToken { _access     :: String
              , _refresh    :: String
              , _expiration :: SystemTime
              } deriving (Show, Eq)

newtype AuthCode = AuthCode String
                 deriving (Show)

isTokenExpired :: AccessToken -> IO Bool
isTokenExpired (AccessToken _ _ expiration) = (expiration <=) <$> getSystemTime

isTokenAlive :: AccessToken -> IO Bool
isTokenAlive = fmap not . isTokenExpired

askToken :: String -> (String -> a) -> ReaderIO Env (Maybe a)
askToken term cons = asks $ fmap cons . M.lookup term

getSecretToken :: ReaderIO Env (Maybe ClientSecret)
getSecretToken = askToken "Client.secret" ClientSecret

getAppId :: ReaderIO Env (Maybe AppId)
getAppId = askToken "App.id" makeAppId
  where
    makeAppId string = AppId (read string :: Integer)

propagateStriveError :: (Response ByteString, String) -> Error
propagateStriveError (full, msg) = errorFromString StriveError $ show full ++ " ; " ++ msg

buildAccessToken :: Text -> Text -> Integer -> AccessToken
buildAccessToken access refresh expires =
  AccessToken (cs access)
              (cs refresh)
              (MkSystemTime (fromIntegral expires) 0)

getAccessToken :: AppId -> ClientSecret -> AuthCode -> IO (Either Error AccessToken)
getAccessToken (AppId appId) (ClientSecret secret) (AuthCode code) =
  exchangeToken appId secret code <&>
    either (Left . propagateStriveError)
           (\ok -> Right $ buildAccessToken (get accessToken ok)
                                            (get refreshToken ok)
                                            (get expiresAt ok))

refreshAccessToken :: AppId -> ClientSecret -> AccessToken -> IO (Either Error AccessToken)
refreshAccessToken (AppId appId) (ClientSecret secret) (AccessToken _ refresh _) =
  refreshExchangeToken appId secret refresh <&>
    either (Left . propagateStriveError)
           (\ok -> Right $ buildAccessToken (get accessToken ok)
                                            (get refreshToken ok)
                                            (get expiresAt ok))
  {-response <- refreshExchangeToken appId secret refresh-}
  {-case response of-}
    {-Left fullmsg -> drats $ propagateStriveError fullmsg-}
    {-Right ok     -> great $ buildAccessToken (get accessToken ok)-}
                                             {-(get refreshToken ok)-}
                                             {-(get expiresAt ok)-}
