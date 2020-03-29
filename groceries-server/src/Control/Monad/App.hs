{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Control.Monad.App where

import           Data.Environment

newtype AppM a = AppM ( ReaderT Environment IO a )

deriving instance Functor AppM
deriving instance Applicative AppM
deriving instance Monad AppM
deriving instance MonadReader Environment AppM
