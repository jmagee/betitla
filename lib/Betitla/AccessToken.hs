module Betitla.AccessToken
( AccessToken (..)
, buildAccessToken
, isTokenExpired
, isTokenAlive
, access
, refresh
, expiration
) where

import           Control.Lens.Getter       ((^.))
import           Data.String.Conversions   (cs)
import           Data.Text                 (Text)
import           Data.Time.Clock.System    (SystemTime (..), getSystemTime)

import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

data AccessToken =
  AccessToken { _access     :: String
              , _refresh    :: String
              , _expiration :: SystemTime
              } deriving (Show, Eq)

-- | Wrapper around SystemTime to avoid orphan instances.
newtype AnySystemTime = AnySystemTime SystemTime

instance Arbitrary AnySystemTime where
  arbitrary = AnySystemTime <$> (MkSystemTime <$> arbitrary <*> arbitrary)

-- | Unwrapper for AnySystemTime.
unwrapAnySystemTime :: AnySystemTime -> SystemTime
unwrapAnySystemTime (AnySystemTime x) = x

instance Arbitrary AccessToken where
  arbitrary = AccessToken <$> arbitrary
                          <*> arbitrary
                          <*> (unwrapAnySystemTime <$> arbitrary)

-- | Access Lens.
access :: Functor f => (String -> f String) -> (AccessToken -> f AccessToken)
access zoom (AccessToken a r e) = fmap (\new -> AccessToken new r e) (zoom a)

-- | Refresh Lens.
refresh :: Functor f => (String -> f String) -> (AccessToken -> f AccessToken)
refresh zoom (AccessToken a r e) = fmap (\new -> AccessToken a new e) (zoom r)

-- | Expieration Lens.
expiration :: Functor f => (SystemTime -> f SystemTime) -> (AccessToken -> f AccessToken)
expiration zoom (AccessToken a r e) = fmap (AccessToken a r) (zoom e)

-- | Check if a token is expired.
-- An expired token will need to be refresh with refreshAccessToken.
isTokenExpired :: AccessToken -> IO Bool
isTokenExpired token = (token ^. expiration <=) <$> getSystemTime

-- | Check if a token is still alive, i.e. not expired.
isTokenAlive :: AccessToken -> IO Bool
isTokenAlive = fmap not . isTokenExpired

-- | Construct an AccessToken from the raw token parts.
buildAccessToken :: Text -> Text -> Integer -> AccessToken
buildAccessToken a r e =
  AccessToken (cs a)
              (cs r)
              (MkSystemTime (fromIntegral e) 0)
