{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
--[># LANGUAGE TypeApplications      #<]
{-# LANGUAGE TypeFamilies          #-}
--[># LANGUAGE TypeSynonymInstances  #<]

module Betitla.Db
( Striver
, StriverT (..)
, BetitlaDb (..)
, betitlaDb
, createDBHandle
, addStriverToDB
, addStriverToDBM
, selectStriverFromDB
, selectStriverFromDBM
, withDB
, withDB'
, dbgDumpDB
, dbgDumpDBM
, updateStriverInDB
, updateStriverInDBM
, deleteStriverFromDB
, deleteStriverFromDBM
) where

import           Betitla.Error
import           Betitla.Striver

import           Data.Functor.Identity   (Identity)
import           Data.Int                (Int32, Int64)
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import           Data.Time.Clock.System  (SystemTime (..))
import           Database.Beam           (Beamable, Columnar, Database,
                                          DatabaseEntity, DatabaseSettings,
                                          MonadBeam, PrimaryKey, Q, QExpr,
                                          Table (..), TableEntity, all_,
                                          defaultDbSettings, delete, guard_,
                                          insert, insertValues, runDelete,
                                          runInsert, runSelectReturningList,
                                          runUpdate, save, select, val_, (==.))
import           Database.Beam.Sqlite    (Sqlite, SqliteM, runBeamSqlite)
import           Database.SQLite.Simple  (Connection, close, execute_, open)
import           GHC.Generics            (Generic)
import           System.Directory        (doesFileExist)

-- | Database table data for a single "Striver".
data StriverT f =
  Striver { --_striverGivenName     :: Columnar f Text
            _striverId            :: Columnar f Int32
          , _striverAccessToken   :: Columnar f Text
          , _striverRefreshToken  :: Columnar f Text
          , _striverExpiration    :: Columnar f Int64
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
  primaryKey = StriverKey . _striverId

newtype BetitlaDb f =
  BetitlaDb { _strivers  :: f (TableEntity StriverT)
            } deriving (Generic, Database Sqlite)

betitlaDb :: DatabaseSettings Sqlite BetitlaDb
betitlaDb = defaultDbSettings

striversTable :: DatabaseEntity Sqlite BetitlaDb (TableEntity StriverT)
striversTable = _strivers betitlaDb

everything :: Q Sqlite BetitlaDb s (StriverT (QExpr Sqlite s))
everything = all_ striversTable

striverFromBits :: AccessToken -> Integer -> Striver
striverFromBits (AccessToken access refresh (MkSystemTime seconds _)) sid =
  Striver (fromInteger sid) (cs access) (cs refresh) seconds

-- | Execute the provided IO function on the provided DB which will
-- be automatically opened and closed.
withDB :: String -> (Connection -> IO a) -> IO a
withDB dbName f = do
  conn <- createDBHandle dbName
  result <- f conn
  close conn
  pure result

-- | Execute the provided Sqlite query on the provided DB which will
-- be automatically opened and closed.
withDB' :: String -> SqliteM a -> IO a
withDB' dbName f = do
  conn <- createDBHandle dbName
  result <- runBeamSqlite conn f
  close conn
  pure result

-- Create a database connection handle.
createDBHandle :: String -> IO Connection
createDBHandle file =
  doesFileExist file >>= \case
    True   -> open file
    False  -> createAndOpen file
  where
    createAndOpen f = do
      conn <- open f
      execute_ conn query
      pure conn
    query = "CREATE TABLE IF NOT EXISTS strivers (id INT32, access_token VARCHAR NOT NULL, refresh_token VARCHAR NOT NULL, expiration INT64, primary key(id))"

    {-createAndOpen file = do-}
      {-conn <- open file-}
      {-execute_ conn "CREATE TABLE IF NOT EXISTS strivers (id INT32, access_token VARCHAR NOT NULL, refresh_token VARCHAR NOT NULL, expiration INT64, primary key(id))"-}
      {---execute_ conn "CREATE TABLE IF NOT EXISTS strivers (given_name VARCHAR NOT NULL, id INT32, access_token VARCHAR NOT NULL, refresh_token VARCHAR NOT NULL, expiration INT64, primary key(id))"-}
      {-pure conn-}

addStriverToDBM :: MonadBeam Sqlite m => AccessToken -> Integer ->  m ()
addStriverToDBM at sid = addStriverToDBM' (striverFromBits at sid)

addStriverToDBM' :: MonadBeam Sqlite m => Striver -> m ()
addStriverToDBM' striver =
  runInsert $ insert striversTable $ insertValues [ striver ]

addStriverToDB :: AccessToken -> Integer -> Connection -> IO ()
addStriverToDB at sid = addStriverToDB' (striverFromBits at sid)

addStriverToDB' :: Striver -> Connection -> IO ()
addStriverToDB' striver conn =
  runBeamSqlite conn $ runInsert $ insert striversTable $ insertValues [ striver ]

-- | Select a user from the database by their Striver ID.
selectStriverFromDBM :: MonadBeam Sqlite m => Integer -> m (Either Error Striver)
selectStriverFromDBM sid =
  extractEither <$> selectStriverFromDBM' (fromInteger sid)
  where
    extractEither [] = Left $ errorFromString DBNotFoundError $ "ID " ++ show sid
    extractEither [x] = Right x
    extractEither x@(_:_) = Left $ errorFromString DBDuplicateError $ show x

selectStriverFromDBM' :: MonadBeam Sqlite m => Int32 -> m [Striver]
selectStriverFromDBM' sid =
  runSelectReturningList $ select $ do
    strivers <- everything
    guard_ (val_ sid ==. _striverId strivers)
    pure strivers

-- | Select a user from the database by their Striver ID.
selectStriverFromDB :: Integer -> Connection -> IO (Either Error Striver)
selectStriverFromDB sid conn =
  extractEither <$> selectStriverFromDB' (fromInteger sid) conn
  where
    extractEither [] = Left $ errorFromString DBNotFoundError $ "ID " ++ show sid
    extractEither [x] = Right x
    extractEither x@(_:_) = Left $ errorFromString DBDuplicateError $ show x

selectStriverFromDB' :: Int32 -> Connection -> IO [Striver]
selectStriverFromDB' sid conn =
  runBeamSqlite conn $ runSelectReturningList $ select $ do
    strivers <- everything
    guard_ (val_ sid ==. _striverId strivers)
    pure strivers

updateStriverInDB :: AccessToken -> Integer -> Connection -> IO ()
updateStriverInDB at sid = updateStriverInDB' (striverFromBits at sid)

updateStriverInDB' :: Striver -> Connection -> IO ()
updateStriverInDB' striver conn =
  runBeamSqlite conn $ do
    runUpdate $
      save striversTable striver

updateStriverInDBM :: MonadBeam Sqlite m => AccessToken -> Integer -> m ()
updateStriverInDBM at sid = updateStriverInDBM' (striverFromBits at sid)

updateStriverInDBM' :: MonadBeam Sqlite m => Striver -> m ()
updateStriverInDBM' striver = runUpdate $ save striversTable striver

deleteStriverFromDB :: Integer -> Connection -> IO ()
deleteStriverFromDB = deleteStriverFromDB' . fromInteger

deleteStriverFromDB' :: Int32 -> Connection -> IO ()
deleteStriverFromDB' sid conn =
  runBeamSqlite conn $ runDelete $
    delete striversTable
           (\x -> _striverId x ==. val_ sid)

deleteStriverFromDBM :: MonadBeam Sqlite m => Integer -> m ()
deleteStriverFromDBM = deleteStriverFromDBM' . fromInteger

deleteStriverFromDBM' :: MonadBeam Sqlite m => Int32 -> m ()
deleteStriverFromDBM' sid =
  runDelete $
    delete striversTable
           (\x -> _striverId x ==. val_ sid)

dbgDumpDB :: Connection -> IO [Striver]
dbgDumpDB conn = runBeamSqlite conn $ runSelectReturningList $ select everything

dbgDumpDBM :: MonadBeam Sqlite m => m [Striver]
dbgDumpDBM = runSelectReturningList $ select everything
