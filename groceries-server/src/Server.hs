{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Server where

import           Servant
import           Servant.API.Generic        ((:-), ToServantApi, genericApi)
import           Servant.Server.Generic     (AsServerT, genericServerT)

import           Network.Wai.Handler.Warp   (run)

import           Control.Monad.Trans.Except (ExceptT (..))

type Constraint r m =
  ( MonadReader r m
  , MonadIO m
  )

newtype API route = API { index :: route :- Get '[JSON] () }
  deriving (Generic)

handler :: Constraint r m => API (AsServerT m)
handler = API { index = pure () }

api :: Proxy (ToServantApi API)
api = genericApi @API Proxy

app :: forall r m. Constraint r m => (forall x. m x -> Servant.Handler x) -> Application
app nat =
  serve api (hoistServer api nat (genericServerT handler))

server :: (MonadUnliftIO m, Constraint r m) => Int -> m ()
server port =
  withRunInIO $ \runInIO  ->
    run port (app (Servant.Handler . ExceptT . try . runInIO))
