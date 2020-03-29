{-# LANGUAGE ConstraintKinds #-}
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
  , DB
  , db
  , migrate

  -- * Public interface
  , insertItem
  , AddItem(..)
  )
  where

import Database.Beam
       ( Database
       , DatabaseSettings
       , TableEntity
       , default_
       , insertExpressions
       , runNoReturn
       , val_
       )
import Database.Beam.Migrate
import Database.Beam.Query.DataTypes
import Database.Beam.Sqlite.Connection
       ( Sqlite
       , SqliteM
       , insertReturning
       , runBeamSqlite
       , runInsertReturningList
       )
import Database.Beam.Sqlite.Migrate
import Database.Beam.Sqlite.Syntax
       (sqliteSerialType)
import Database.SQLite.Simple
       (Connection)

import Control.Lens
       (Lens', lens, set, view)

import Data.Groceries
import Data.User

import Data.GenValidity
import Data.GenValidity.Text
       ()
import Data.GenValidity.Time
       ()

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

data DB f
  = DB { _databaseItems :: f (TableEntity ItemT)
       , _databaseUsers :: f (TableEntity UserT)
       }

deriving stock instance Generic (DB f)
deriving anyclass instance (Database be) DB

db :: DatabaseSettings Sqlite DB
db = unCheckDatabase (evaluateDatabase migration)

migrate :: WithDB r m => m ()
migrate = void $ runDB $
  runMigrationSteps 0 Nothing migration (\_n _comment -> executeMigration runNoReturn)


migration :: MigrationSteps Sqlite () (CheckedDatabaseSettings Sqlite DB)
migration = migrationStep "Initial commit" v1
  where
    v1 :: () -> Migration Sqlite (CheckedDatabaseSettings Sqlite DB)
    v1 _ = DB <$> createTable "items" itemT
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

data AddItem
  = AddItem { addItemName :: Text
            , addItemQuantity :: Int64
            , addItemWanted :: Int64
            , addItemExpires :: Maybe Day
            }
  deriving (Show, Eq, Ord, Generic)

instance Validity AddItem
instance GenValid AddItem where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

insertItem :: WithDB r m => AddItem -> m (Maybe Item)
insertItem AddItem{..} = fmap listToMaybe $ runDB $ runInsertReturningList $
  insertReturning (_databaseItems db) $ insertExpressions $
  let item = Item { _itemId = default_
                  , _itemName = val_ addItemName
                  , _itemQuantity = val_ addItemQuantity
                  , _itemWanted = val_ addItemWanted
                  , _itemExpires = val_ addItemExpires
                  }
  in [ item ]
