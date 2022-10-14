{-# LANGUAGE TemplateHaskell #-}

module Betitla.Lenses
( access
, refresh
, expiration
) where

import           Betitla.Striver

import           Control.Lens    (makeLenses)

makeLenses ''AccessToken
