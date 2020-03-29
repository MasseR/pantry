{-# LANGUAGE RecordWildCards #-}
module Test.Database.Groceries where

import Test.Hspec

import Database.Beam
import Database.SQLite.Simple
       (Connection, withConnection)

import Data.Groceries
-- System under test
import Database.Groceries

withDB :: (Connection -> IO a) -> IO a
withDB = withConnection ":memory:"

setup :: (Connection -> IO a) -> IO a
setup f = withConnection ":memory:" $ \conn -> do
  runReaderT migrate conn
  f conn

packItem :: AddItem -> SqlSerial Int -> Item
packItem AddItem{..} identifier =
  Item { _itemId = identifier
       , _itemName = addItemName
       , _itemQuantity = addItemQuantity
       , _itemWanted = addItemWanted
       , _itemExpires = addItemExpires
       }

spec :: Spec
spec = describe "Database.Groceries" $ do
  around withDB $ context "Migrations" $
    it "Successfully migrates" $ \conn -> do
      got <- runReaderT migrate conn
      got `shouldBe` ()
  around setup $ context "Dummu queries" $
    it "Successfully cruds" $ \conn -> do
      let addItem = AddItem "milk" 1 3 Nothing
      got <- runReaderT (insertItem addItem) conn
      let wanted = packItem addItem <$> (fmap _itemId got)
      got `shouldBe` wanted
