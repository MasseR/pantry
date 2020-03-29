module Database.Groceries
  ( HasConnection(..)
  , runDB
  )
  where

import           Database.Beam.Sqlite.Connection (SqliteM, runBeamSqlite)
import           Database.SQLite.Simple          (Connection)

import           Control.Lens                    (Lens', lens, set, view)

class HasConnection a where
  {-# MINIMAL getConnection, setConnection | connection #-}
  getConnection :: a -> Connection
  getConnection = view connection

  setConnection :: a -> Connection -> a
  setConnection = flip (set connection)

  connection :: Lens' a Connection
  connection = lens getConnection setConnection

runDB :: (MonadReader r m, HasConnection r, MonadIO m) => SqliteM a -> m a
runDB f = view connection >>= \conn -> liftIO (runBeamSqlite conn f)
