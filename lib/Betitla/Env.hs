module Betitla.Env
( Env
, getEnvRC
, getEnvRCFrom
) where

import           Betitla.Util

import           Data.Aeson   (eitherDecode')
import           Data.Map     (Map, empty)
import           Path         (Abs, File, Path, parseAbsFile)

type Env = Map String String

-- | Get Env from the RC file
getEnvRC :: IO Env
getEnvRC = mkHomePath ".betitla.rc" >>= parseAbsFile >>= readEnvRC

-- | Like getEnvRC, but use the specified filepath to find the rc
getEnvRCFrom :: Path Abs File -> IO Env
getEnvRCFrom = readEnvRC

readEnvRC :: Path Abs File -> IO Env
readEnvRC file = unlessEmpty file empty $ \contents ->
  either (jbail file) id (eitherDecode' contents :: Either String Env)
