{-# LANGUAGE RecordWildCards #-}
module Database.Groceries.Items
  ( insertItem
  )
  where

import Database.Beam
import Database.Beam.Sqlite

import Data.Item
import Database.Groceries

insertItem :: WithDB r m => AddItem -> m (Maybe Item)
insertItem AddItem{..} = fmap listToMaybe $ runDB $ runInsertReturningList $
  insertReturning (db ^. items) $ insertExpressions $
  let item = Item { _itemId = default_
                  , _itemName = val_ addItemName
                  , _itemQuantity = val_ addItemQuantity
                  , _itemWanted = val_ addItemWanted
                  , _itemExpires = val_ addItemExpires
                  }
  in [ item ]
