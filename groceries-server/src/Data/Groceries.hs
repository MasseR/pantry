{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Groceries where

import           Database.Beam

data Quantity = Quantity
              deriving (Eq, Ord, Show)

-- Testing out the model
data ItemT f
  = Item { _itemId       :: Columnar f Int64
         , _itemName     :: Columnar f Text
         , _itemQuantity :: Columnar f Quantity
         , _itemWanted   :: Columnar f Quantity
         , _itemExpires  :: Columnar f (Maybe Day)
         }

type Item = ItemT Identity

deriving instance Generic (ItemT f)
deriving instance Show Item
deriving instance Eq Item
deriving instance Ord Item
deriving instance Beamable ItemT

instance Table ItemT where
  data PrimaryKey ItemT f = ItemId (Columnar f Int64)
  primaryKey = ItemId . _itemId

deriving instance Generic (PrimaryKey ItemT f)
deriving instance Beamable (PrimaryKey ItemT)
deriving instance Show (PrimaryKey ItemT Identity)
deriving instance Eq (PrimaryKey ItemT Identity)
deriving instance Ord (PrimaryKey ItemT Identity)
