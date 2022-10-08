{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Betitla.Db
( Striver (..)
, StriverT (..)
, BetitlaDb (..)
, betitlaDb
) where

import           Betitla.Striver

import           Data.Int      (Int32, Int64)
import           Data.Sequence (Seq (..))
import           Data.Text     (Text)
import           Database.Beam
import           Database.Beam.Sqlite

data StriverT f =
  Striver { _striverGivenName     :: Columnar f Text
          , _striverId            :: Columnar f Int32
          , _striverAccessToken   :: Columnar f Text
          , _striverRefreshToken  :: Columnar f Text
          , _striverExpiration    :: Int64
          --, _accessToken   :: Columnar f AccessToken
          --, _procActivites :: Columnar f (Seq Integer)  -- do we even need this one?  maybe not
          } deriving (Generic, Beamable)

type Striver = StriverT Identity
type StriverKey = PrimaryKey StriverT Identity

deriving instance Show Striver
deriving instance Eq Striver

instance Table StriverT where
  data PrimaryKey StriverT f = StriverKey (Columnar f Int32)
                             deriving (Generic, Beamable)
  --primaryKey = StriverKey . _id
  primaryKey = StriverKey . _striverId

data BetitlaDb f =
  BetitlaDb { _strivers  :: f (TableEntity StriverT)
            } deriving (Generic, Database Sqlite)

betitlaDb :: DatabaseSettings Sqlite BetitlaDb
betitlaDb = defaultDbSettings
