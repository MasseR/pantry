{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Control.Monad.App where

import           Data.Environment

newtype AppM a = AppM ( ReaderT Environment IO a )

deriving instance Functor AppM
deriving instance Applicative AppM
deriving instance Monad AppM
deriving instance MonadIO AppM
deriving instance MonadUnliftIO AppM
deriving instance MonadReader Environment AppM

runAppM :: Environment -> AppM a -> IO a
runAppM env (AppM f) = runReaderT f env
