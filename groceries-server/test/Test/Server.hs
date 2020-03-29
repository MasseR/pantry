{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Test.Server where

import Control.Monad.App

import Test.Hspec

import Database.SQLite.Simple
       (withConnection)

import Network.Wai.Handler.Warp
       (testWithApplication)

import Control.Monad.Trans.Except
       (ExceptT(..))

import Network.HTTP.Client
       (Manager, defaultManagerSettings, newManager)
import Servant
       (Handler(..))
import Servant.Client
       (ClientError, ClientM, mkClientEnv, runClientM)
import Servant.Client.Core
       (BaseUrl(..), parseBaseUrl)
import Servant.Client.Generic
       (AsClientT, genericClient)

import Control.Lens
       (set)
import Data.Generics.Product
       (field)

import Data.Environment

-- System under test
import Server
       (API(..), app)

-- * Set up tests
data TestEnvironment
  = TestEnvironment { testBaseUrl     :: BaseUrl
                    , testManager     :: Manager
                    , testEnvironment :: Environment
                    }
  deriving Generic

runClient :: (MonadReader TestEnvironment m, MonadIO m) => ClientM a -> m (Either ClientError a)
runClient c = do
  TestEnvironment{..} <- ask
  let env = mkClientEnv testManager testBaseUrl
  liftIO $ runClientM c env

withServer :: (TestEnvironment -> IO a) -> IO a
withServer f = withConnection ":memory:" $ \conn -> do
  let env = Environment conn
  testWithApplication (pure (app (nat env))) $ \p -> do
    let makeBaseUrl = set (field @"baseUrlPort") p <$> parseBaseUrl "http://localhost"
    testEnv <- TestEnvironment <$> makeBaseUrl <*> newManager defaultManagerSettings <*> pure env
    f testEnv
  where
    nat env = Servant.Handler . ExceptT . try . runAppM env

client :: API (AsClientT ClientM)
client = genericClient

spec :: Spec
spec = around withServer $ describe "Server" $
  it "Returns unit for index" $ \env -> do
    got <- runReaderT (runClient (index client)) env
    got `shouldBe` Right ()
