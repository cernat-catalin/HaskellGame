{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module GMessages.Network.ClientServer (
  ConnectionMessage(..),
  PingMessage(..),
  WorldMessage(..),
  ServiceMessage(..),
  Message(..)
  ) where

import Data.Serialize (Serialize, get, put, Get, Putter)
import GHC.Generics (Generic)
import qualified Linear as L
import Graphics.Rendering.OpenGL (GLfloat)
import Data.Time.Clock (UTCTime(..), DiffTime(..))
import Data.Time.Calendar (Day(..))

import GCommon.Types.Generic (PlayerSettings, PlayerSettingsReset, ClientKey)



data ConnectionMessage = ConnectionRequest PlayerSettings
                       | ConnectionTerminated
                       deriving (Show, Eq, Generic)

data PingMessage = PingRequest UTCTime
                 deriving (Show, Eq, Generic)

data WorldMessage = PositionUpdate (L.V2 GLfloat, GLfloat)
                  | SettingsUpdate PlayerSettings
                  | SettingsReset PlayerSettingsReset
                  | Fire
                  deriving (Show, Eq, Generic)

data ServiceMessage = PingMessage PingMessage
                    | ConnectionMessage ConnectionMessage
                    deriving (Show, Eq, Generic)

data Message = WorldMessage ClientKey WorldMessage
             | ServiceMessage ClientKey ServiceMessage
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
instance Serialize WorldMessage
instance Serialize ServiceMessage
instance Serialize Message
