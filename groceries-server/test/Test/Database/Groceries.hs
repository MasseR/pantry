{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Database.Groceries where

import Prelude hiding
       (assert)

import Data.GenValidity
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

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

newtype Commands = InsertItem AddItem
  deriving (Show, Eq, Ord)

instance Arbitrary Commands where
  arbitrary = oneof [ InsertItem <$> genValid ]

execInsertItem :: AddItem -> PropertyM (ReaderT Connection IO) ()
execInsertItem new = do
  got <- run $ insertItem new
  let wanted = packItem new <$> fmap _itemId got
  assert (got == wanted)

prop_database :: Connection -> [Commands] -> Property
prop_database conn commands =
  monadic exec $ for_ commands $ \case
    InsertItem new -> execInsertItem new
  where
    exec f = ioProperty $ runReaderT f conn

spec :: Spec
spec = describe "Database.Groceries" $ do
  around withDB $ context "Migrations" $
    it "Successfully migrates" $ \conn -> do
      got <- runReaderT migrate conn
      got `shouldBe` ()
  around setup $ context "Dummu queries" $ do
    it "Successfully cruds" $ \conn -> do
      let addItem = AddItem "milk" 1 3 Nothing
      got <- runReaderT (insertItem addItem) conn
      let wanted = packItem addItem <$> fmap _itemId got
      got `shouldBe` wanted
    it "Follows sane commands" $ \conn -> property (prop_database conn)
