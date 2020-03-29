{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
module Database.Groceries
  ( HasConnection(..)
  , runDB
  , WithDB
  , db
  , migrationSteps

  , (^.)
  , items
  , users
  )
  where

import Database.Beam
       ( Database
       , DatabaseSettings
       , TableEntity
       )
import Database.Beam.Migrate
import Database.Beam.Query.DataTypes
import Database.Beam.Sqlite.Connection
       ( Sqlite
       , SqliteM
       , runBeamSqlite
       )
import Database.Beam.Sqlite.Migrate
import Database.Beam.Sqlite.Syntax
       (sqliteSerialType)
import Database.SQLite.Simple
       (Connection)

import Control.Lens.TH (makeLenses)
import Control.Lens
       (Lens', lens, set, view, (^.))

import Data.Item
import Data.User


class HasConnection a where
  {-# MINIMAL getConnection, setConnection | connection #-}
  getConnection :: a -> Connection
  getConnection = view connection

  setConnection :: a -> Connection -> a
  setConnection = flip (set connection)

  connection :: Lens' a Connection
  connection = lens getConnection setConnection

instance HasConnection Connection where
  getConnection = id
  setConnection = const


type WithDB r m = (MonadReader r m, HasConnection r, MonadIO m)

runDB :: WithDB r m => SqliteM a -> m a
runDB f = view connection >>= \conn -> liftIO (runBeamSqlite conn f)

data Groceries f
  = Groceries { _items :: f (TableEntity ItemT)
              , _users :: f (TableEntity UserT)
              }

deriving stock instance Generic (Groceries f)
deriving anyclass instance (Database be) Groceries

makeLenses ''Groceries

db :: DatabaseSettings Sqlite Groceries
db = unCheckDatabase (evaluateDatabase migrationSteps)

migrationSteps :: MigrationSteps Sqlite () (CheckedDatabaseSettings Sqlite Groceries)
migrationSteps = migrationStep "Initial commit" v1
  where
    v1 :: () -> Migration Sqlite (CheckedDatabaseSettings Sqlite Groceries)
    v1 _ = Groceries <$> createTable "items" itemT
                     <*> createTable "users" userT
    itemT :: TableSchema Sqlite ItemT
    itemT = Item { _itemId = field "item_id" (DataType sqliteSerialType)
                 , _itemName = field "name" sqliteText
                 , _itemQuantity = field "quantity" sqliteBigInt
                 , _itemWanted = field "wanted" sqliteBigInt
                 , _itemExpires = field "expires" (maybeType date)
                 }
    userT :: TableSchema Sqlite UserT
    userT = User { _userId = field "user_id" (DataType sqliteSerialType)
                 , _userName = field "username" sqliteText
                 , _userPassword = field "password" sqliteBlob
                 }

