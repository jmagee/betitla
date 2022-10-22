{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Betitla.Lenses where
{-( access-}
{-, refresh-}
{-, expiration-}
{-) where-}

import           Betitla.AccessToken
import           Betitla.ActivityRating

import           Control.Lens    (makeLenses)

makeLenses ''AccessToken
makeLenses ''ActivityRating
