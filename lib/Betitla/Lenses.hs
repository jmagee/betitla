{-# LANGUAGE TemplateHaskell #-}

module Betitla.Lenses
( access
, refresh
, expiration
) where

import           Betitla.AccessToken

import           Control.Lens    (makeLenses)

makeLenses ''AccessToken
