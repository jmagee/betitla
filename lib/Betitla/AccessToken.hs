module Betitla.AccessToken
( AccessToken (..)
, isTokenExpired
, isTokenAlive
) where

import           Data.Time.Clock.System (SystemTime (..), getSystemTime)

data AccessToken =
  AccessToken { _access     :: String
              , _refresh    :: String
              , _expiration :: SystemTime
              } deriving (Show, Eq)

-- | Check if a token is expired.
-- An expired token will need to be refresh with refreshAccessToken.
isTokenExpired :: AccessToken -> IO Bool
isTokenExpired (AccessToken _ _ expiration) = (expiration <=) <$> getSystemTime

-- | Check if a token is still alive, i.e. not expired.
isTokenAlive :: AccessToken -> IO Bool
isTokenAlive = fmap not . isTokenExpired
