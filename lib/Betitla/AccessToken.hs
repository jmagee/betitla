{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

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
import           Data.Text                 (Text)
import           Data.Time.Clock.System    (SystemTime (..), getSystemTime)
import           Witch                     (From, from)

import           Test.QuickCheck           (Gen)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

data AccessToken =
  AccessToken { _access     :: Text
              , _refresh    :: Text
              , _expiration :: SystemTime
              } deriving (Show, Eq)

-- | Wrapper around SystemTime to avoid orphan instances.
newtype AnySystemTime = AnySystemTime SystemTime

instance Arbitrary AnySystemTime where
  arbitrary = AnySystemTime <$> (MkSystemTime <$> arbitrary <*> arbitrary)

instance From AnySystemTime SystemTime where
  from (AnySystemTime x) = x

-- | Text Wrapper for testing.
newtype AnyText = AnyText Text

instance Arbitrary AnyText where
  arbitrary = AnyText . from <$> (arbitrary :: Gen String)

instance From AnyText Text where
  from (AnyText x) = x

instance Arbitrary AccessToken where
  arbitrary = AccessToken <$> (from @AnyText <$> arbitrary)
                          <*> (from @AnyText <$> arbitrary)
                          <*> (from @AnySystemTime <$> arbitrary)

-- | Access Lens.
access :: Functor f => (Text -> f Text) -> (AccessToken -> f AccessToken)
access zoom (AccessToken a r e) = fmap (\new -> AccessToken new r e) (zoom a)

-- | Refresh Lens.
refresh :: Functor f => (Text -> f Text) -> (AccessToken -> f AccessToken)
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
buildAccessToken a r e = AccessToken a r (MkSystemTime (fromIntegral e) 0)
