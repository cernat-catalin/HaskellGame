{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module GMessages.Network.ServerClient (
  ConnectionMessage(..),
  PingMessage(..),
  SettingsMessage(..),
  WorldMessage(..),
  ServiceMessage(..),
  Message(..)
  ) where

import Data.Serialize (Serialize, get, put, Get, Putter)
import GHC.Generics (Generic)
import Data.Time.Clock (UTCTime(..), DiffTime(..))
import Data.Time.Calendar (Day(..))

import GCommon.Types.Generic (ClientKey)
import GCommon.Objects.Objects (World)


data ConnectionMessage = PlayerKey ClientKey
                       deriving (Show, Eq, Generic)

data PingMessage = PingResponse UTCTime
                 deriving (Show, Eq, Generic)

data SettingsMessage = Dead
                     deriving (Show, Eq, Generic)

data WorldMessage = WorldUpdate World
                  deriving (Show, Eq, Generic)

data ServiceMessage = PingMessage PingMessage
                    | SettingsMessage SettingsMessage
                    deriving (Show, Eq, Generic)

data Message = WorldMessage WorldMessage
             | ServiceMessage ServiceMessage
             deriving (Show, Eq, Generic)

deriving instance Generic UTCTime
deriving instance Generic Day
instance Serialize DiffTime where
  get = realToFrac <$> (get :: Get Double)
  put = (put :: Putter Double) . realToFrac
instance Serialize Day
instance Serialize UTCTime

instance Serialize ConnectionMessage
instance Serialize PingMessage
instance Serialize SettingsMessage
instance Serialize ServiceMessage
instance Serialize WorldMessage
instance Serialize Message