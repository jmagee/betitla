module Betitla.Env
( Env
, getEnvRC
, getEnvRCFrom
) where

import           Betitla.Util

import           Data.Aeson   (eitherDecode')
import           Data.Map     (Map, empty)
import           Data.Text    (Text)
import           Path         (Abs, File, Path, parseAbsFile)

-- | The environment type is simply a map of string pairs.
type Env = Map Text Text

-- | Get Env from the RC file
getEnvRC :: IO Env
getEnvRC = mkHomePath ".betitla.rc" >>= parseAbsFile >>= readEnvRC

-- | Like getEnvRC, but use the specified filepath to find the rc
getEnvRCFrom :: Path Abs File -> IO Env
getEnvRCFrom = readEnvRC

-- | Read the environment RC file.
-- This function is partial - if the JSON file cannot be parsed then it bails.
readEnvRC :: Path Abs File -> IO Env
readEnvRC file = unlessEmpty file empty $ \contents ->
  either (jbail file) id (eitherDecode' contents :: Either String Env)
