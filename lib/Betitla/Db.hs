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

module Betitla.Db
( Striver
, StriverT (..)
, BetitlaDb (..)
, betitlaDb
, createDbHandle
, accessTokenFromStriver
, addStriverToDb
, selectStriverFromDb
, withDb
, dbgDumpDb
, updateStriverInDb
, deleteStriverFromDb
) where

import           Betitla.Error
import           Betitla.Lenses
import           Betitla.Striver

import           Control.Lens.Getter     ((^.))
import           Data.Functor.Identity   (Identity)
import           Data.Int                (Int32, Int64)
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

-- | Database table data for a single "Striver".
data StriverT f =
  Striver { _striverId            :: Columnar f Int32
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
--striverFromBits (AccessToken access refresh (MkSystemTime seconds _)) sid =
  --Striver (fromInteger sid) (cs access) (cs refresh) seconds
striverFromBits token sid =
  Striver (fromInteger sid)
          (cs $ token ^. access)
          (cs $ token ^. refresh)
          (systemSeconds $ token ^. expiration)

accessTokenFromStriver :: Striver -> AccessToken
accessTokenFromStriver striver = 
  buildAccessToken (striver ^. striverAccessToken)
                   (striver ^. striverRefreshToken)
                   (toInteger $ striver ^. striverExpiration)

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
      pure conn
    query = "CREATE TABLE IF NOT EXISTS strivers (id INT32, access_token VARCHAR NOT NULL, refresh_token VARCHAR NOT NULL, expiration INT64, primary key(id))"

addStriverToDb :: MonadBeam Sqlite m => AccessToken -> Integer ->  m ()
addStriverToDb at sid = addStriverToDb' (striverFromBits at sid)

addStriverToDb' :: MonadBeam Sqlite m => Striver -> m ()
addStriverToDb' striver =
  runInsert $ insert striversTable $ insertValues [ striver ]

-- | Select a user from the database by their Striver ID.
selectStriverFromDb :: MonadBeam Sqlite m => Integer -> m (Either Error Striver)
selectStriverFromDb sid =
  extractEither <$> selectStriverFromDb' (fromInteger sid)
  where
    extractEither [] = Left $ errorFromString DBNotFoundError $ "ID " ++ show sid
    extractEither [x] = Right x
    extractEither x@(_:_) = Left $ errorFromString DBDuplicateError $ show x

selectStriverFromDb' :: MonadBeam Sqlite m => Int32 -> m [Striver]
selectStriverFromDb' sid =
  runSelectReturningList $ select $ do
    strivers <- everything
    guard_ (val_ sid ==. _striverId strivers)
    pure strivers

updateStriverInDb :: MonadBeam Sqlite m => AccessToken -> Integer -> m ()
updateStriverInDb at sid = updateStriverInDb' (striverFromBits at sid)

updateStriverInDb' :: MonadBeam Sqlite m => Striver -> m ()
updateStriverInDb' striver = runUpdate $ save striversTable striver

deleteStriverFromDb :: MonadBeam Sqlite m => Integer -> m ()
deleteStriverFromDb = deleteStriverFromDb' . fromInteger

deleteStriverFromDb' :: MonadBeam Sqlite m => Int32 -> m ()
deleteStriverFromDb' sid =
  runDelete $
    delete striversTable
           (\x -> _striverId x ==. val_ sid)

dbgDumpDb :: MonadBeam Sqlite m => m [Striver]
dbgDumpDb = runSelectReturningList $ select everything
