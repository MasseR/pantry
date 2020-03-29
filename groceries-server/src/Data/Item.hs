{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Item
  ( ItemT(..)
  , Item
  , SqlSerial
  , AddItem(..)
  )
  where

import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions

import Data.GenValidity
import Data.GenValidity.Text
       ()
import Data.GenValidity.Time
       ()

-- Testing out the model
data ItemT f
  = Item { _itemId       :: Columnar f (SqlSerial Int)
         , _itemName     :: Columnar f Text
         , _itemQuantity :: Columnar f Int64
         , _itemWanted   :: Columnar f Int64
         , _itemExpires  :: Columnar f (Maybe Day)
         }

type Item = ItemT Identity

deriving instance Generic (ItemT f)
deriving instance Show Item
deriving instance Eq Item
deriving instance Ord Item
deriving instance Beamable ItemT

instance Table ItemT where
  data PrimaryKey ItemT f = ItemId (Columnar f (SqlSerial Int))
  primaryKey = ItemId . _itemId

deriving instance Generic (PrimaryKey ItemT f)
deriving instance Beamable (PrimaryKey ItemT)
deriving instance Show (PrimaryKey ItemT Identity)
deriving instance Eq (PrimaryKey ItemT Identity)
deriving instance Ord (PrimaryKey ItemT Identity)

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
