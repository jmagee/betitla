{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
--[># LANGUAGE TypeApplications      #<]
{-# LANGUAGE TypeFamilies          #-}
--[># LANGUAGE TypeSynonymInstances  #<]
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
-- ^ tableLenses signatures seem problematic to express

module Betitla.Db
( Striver
, StriverT (..)
, Activity
, ActivityT (..)
, BetitlaDb (..)
, betitlaDb
, createDbHandle
, accessTokenFromStriver
, addStriverToDb
, selectStriverFromDb
, withDb
, dbgDumpStrivers
, dbgDumpActivities
, updateStriverInDb
, deleteStriverFromDb
, addActivityToDb
, selectActivitiesFromDb
, selectActivityFromDb
, activityId
, doesStriverExistInDb
, doesActivityExistInDb
) where

import           Betitla.AccessToken
import           Betitla.Error
import           Betitla.StriverIds

import           Control.Lens.Getter     ((^.))
import           Data.Functor.Identity   (Identity)
import           Data.Int                (Int64)
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import           Data.Time.Clock.System  (SystemTime (..))
import           Database.Beam           (Beamable, Columnar, Database,
                                          DatabaseEntity, DatabaseSettings,
                                          LensFor (..), MonadBeam, PrimaryKey,
                                          Q, QExpr, Table (..), TableEntity,
                                          all_, defaultDbSettings, delete,
                                          guard_, insert, insertValues,
                                          runDelete, runInsert,
                                          runSelectReturningList, runUpdate,
                                          save, select, tableLenses, val_,
                                          (==.))
import           Database.Beam.Sqlite    (Sqlite, SqliteM, runBeamSqlite)
import           Database.SQLite.Simple  (Connection, close, execute_, open)
import           GHC.Generics            (Generic)
import           System.Directory        (doesFileExist)
import           Witch (into, from)

-- | Database table data for a single "Striver".
-- This records the striver ID, access and refresh tokens, and the refresh token expiration
data StriverT f =
  Striver { _striverId            :: Columnar f Int64
          , _striverAccessToken   :: Columnar f Text
          , _striverRefreshToken  :: Columnar f Text
          , _striverExpiration    :: Columnar f Int64
          } deriving (Generic, Beamable)

Striver (LensFor striverId) (LensFor striverAccessToken)
        (LensFor striverRefreshToken) (LensFor striverExpiration)
        = tableLenses

type Striver = StriverT Identity
type StriverKey = PrimaryKey StriverT Identity

deriving instance Show Striver
deriving instance Eq Striver

instance Table StriverT where
  data PrimaryKey StriverT f = StriverKey (Columnar f Int64)
                             deriving (Generic, Beamable)
  primaryKey = StriverKey . _striverId

-- | Database table data for striver activities.
data ActivityT f =
  Activity   { _activityStriverId  :: Columnar f Int64
             , _activityId         :: Columnar f Int64
             } deriving (Generic, Beamable)

Activity (LensFor activityStriverId) (LensFor activityId) = tableLenses

type Activity = ActivityT Identity
type ActivityKey = PrimaryKey ActivityT Identity

deriving instance Show Activity
deriving instance Eq Activity

instance Table ActivityT where
  data PrimaryKey ActivityT f = ActivityKey (Columnar f Int64)
                                deriving (Generic, Beamable)
  primaryKey = ActivityKey . _activityId

data BetitlaDb f =
  BetitlaDb { _strivers   :: f (TableEntity StriverT)
            , _activities :: f (TableEntity ActivityT)
            } deriving (Generic, Database Sqlite)

betitlaDb :: DatabaseSettings Sqlite BetitlaDb
betitlaDb = defaultDbSettings

striversTable :: DatabaseEntity Sqlite BetitlaDb (TableEntity StriverT)
striversTable = _strivers betitlaDb

activitiesTable :: DatabaseEntity Sqlite BetitlaDb (TableEntity ActivityT)
activitiesTable = _activities betitlaDb

allStrivers :: Q Sqlite BetitlaDb s (StriverT (QExpr Sqlite s))
allStrivers = all_ striversTable

allActivities :: Q Sqlite BetitlaDb s (ActivityT (QExpr Sqlite s))
allActivities = all_ activitiesTable

striverFromBits :: AccessToken -> AthleteId -> Striver
striverFromBits token sid =
  Striver (from sid)
          (cs $ token ^. access)
          (cs $ token ^. refresh)
          (systemSeconds $ token ^. expiration)

accessTokenFromStriver :: Striver -> AccessToken
accessTokenFromStriver striver =
  buildAccessToken (striver ^. striverAccessToken)
                   (striver ^. striverRefreshToken)
                   (into $ striver ^. striverExpiration)

-- | Execute the provided Sqlite query on the provided DB which will
-- be automatically opened and closed.
withDb :: String -> SqliteM a -> IO a
withDb dbName f = do
  conn <- createDbHandle dbName
  result <- runBeamSqlite conn f
  close conn
  pure result

-- Create a database connection handle.
createDbHandle :: String -> IO Connection
createDbHandle file =
  doesFileExist file >>= \case
    True   -> open file
    False  -> createAndOpen file
  where
    createAndOpen f = do
      conn <- open f
      execute_ conn query
      execute_ conn query2
      pure conn
    query = "CREATE TABLE IF NOT EXISTS strivers (id INT64, access_token VARCHAR NOT NULL, refresh_token VARCHAR NOT NULL, expiration INT64, primary key(id))"
    query2 = "CREATE TABLE IF NOT EXISTS activities (striver_id INT64, id INT64, primary key(id))"

addStriverToDb :: MonadBeam Sqlite m => AccessToken -> AthleteId -> m ()
addStriverToDb at sid = addStriverToDb' (striverFromBits at (from sid))

addStriverToDb' :: MonadBeam Sqlite m => Striver -> m ()
addStriverToDb' striver =
  runInsert $ insert striversTable $ insertValues [ striver ]

-- | Select a user from the database by their Striver ID.
selectStriverFromDb :: MonadBeam Sqlite m => AthleteId-> m (Either Error Striver)
selectStriverFromDb sid =
  extractEither <$> selectStriverFromDb' (from sid)
  where
    extractEither [] = Left $ errorFromString DBNotFoundError $ "ID " ++ show sid
    extractEither [x] = Right x
    extractEither x@(_:_) = Left $ errorFromString DBDuplicateError $ show x

selectStriverFromDb' :: MonadBeam Sqlite m => Int64 -> m [Striver]
selectStriverFromDb' sid =
  runSelectReturningList $ select $ do
    strivers <- allStrivers
    guard_ (val_ sid ==. _striverId strivers)
    pure strivers

-- | Determine if a striver exists in the database.
doesStriverExistInDb :: MonadBeam Sqlite m => AthleteId -> m Bool
doesStriverExistInDb x = selectStriverFromDb x >>= \case
  Left _  -> pure False
  Right _ -> pure True

updateStriverInDb :: MonadBeam Sqlite m => AccessToken -> AthleteId -> m ()
updateStriverInDb at sid = updateStriverInDb' (striverFromBits at sid)

updateStriverInDb' :: MonadBeam Sqlite m => Striver -> m ()
updateStriverInDb' striver = runUpdate $ save striversTable striver

deleteStriverFromDb :: MonadBeam Sqlite m => AthleteId -> m ()
deleteStriverFromDb = deleteStriverFromDb' . into @Int64

deleteStriverFromDb' :: MonadBeam Sqlite m => Int64 -> m ()
deleteStriverFromDb' sid =
  runDelete $
    delete striversTable
           (\x -> x ^. striverId ==. val_ sid)

dbgDumpStrivers :: MonadBeam Sqlite m => m [Striver]
dbgDumpStrivers = runSelectReturningList $ select allStrivers

addActivityToDb :: MonadBeam Sqlite m => AthleteId -> ActivityId -> m ()
addActivityToDb sid aid =
  runInsert $
    insert activitiesTable $
      insertValues [ Activity (into @Int64 sid) (into @Int64 aid) ]

selectActivitiesFromDb :: MonadBeam Sqlite m => AthleteId -> m (Either Error [Activity])
selectActivitiesFromDb sid = extractEither <$> selectActivitiesFromDb' (into @Int64 sid)
  where
    extractEither [] = Left $ errorFromString DBNotFoundError $ "ID " ++ show sid
    extractEither x  = Right x

selectActivitiesFromDb' :: MonadBeam Sqlite m => Int64 -> m [Activity]
selectActivitiesFromDb' sid =
  runSelectReturningList $ select $ do
    activities <- allActivities
    guard_ (val_ sid ==. activities ^. activityStriverId)
    pure activities

-- | Select a single activity by activity ID.
selectActivityFromDb :: MonadBeam Sqlite m => ActivityId -> m (Either Error Activity)
selectActivityFromDb sid =
  extractEither <$> selectActivityFromDb' (into @Int64 sid)
  where
    extractEither [] = Left $ errorFromString DBNotFoundError $ "ID " ++ show sid
    extractEither [x] = Right x
    extractEither x@(_:_) = Left $ errorFromString DBDuplicateError $ show x

selectActivityFromDb' :: MonadBeam Sqlite m => Int64 -> m [Activity]
selectActivityFromDb' sid =
  runSelectReturningList $ select $ do
    activities <- allActivities
    guard_ (val_ sid ==. activities ^. activityId)
    pure activities

-- | Determine if an activity exists in the database.
doesActivityExistInDb :: MonadBeam Sqlite m => ActivityId -> m Bool
doesActivityExistInDb x = selectActivityFromDb x >>= \case
  Left _  -> pure False
  Right _ -> pure True

dbgDumpActivities :: MonadBeam Sqlite m => m [Activity]
dbgDumpActivities = runSelectReturningList $ select allActivities
