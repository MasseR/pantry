{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Data.User where

import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions

data UserT f
  = User { _userId :: Columnar f (SqlSerial Int)
         , _userName :: Columnar f Text
         , _userPassword :: Columnar f ByteString
         }

type User = UserT Identity

deriving instance Generic (UserT f)
deriving instance Show User
deriving instance Eq User
deriving instance Ord User
deriving instance Beamable UserT

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f (SqlSerial Int))
  primaryKey = UserId . _userId

deriving instance Generic (PrimaryKey UserT f)
deriving instance Beamable (PrimaryKey UserT)
deriving instance Show (PrimaryKey UserT Identity)
deriving instance Eq (PrimaryKey UserT Identity)
deriving instance Ord (PrimaryKey UserT Identity)
