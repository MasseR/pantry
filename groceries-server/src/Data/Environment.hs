{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
module Data.Environment where

import           Database.SQLite.Simple (Connection)

import           Database.Groceries     (HasConnection (connection))

import           Data.Generics.Product  (typed)

newtype Environment = Environment { _connection :: Connection }
  deriving (Generic)

instance HasConnection Environment where
  connection = typed @Connection
