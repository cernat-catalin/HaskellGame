{-# LANGUAGE DeriveGeneric #-}

module GMessages.Network.ClientServer (
  ConnectionMessage(..),
  PingMessage(..),
  WorldMessage(..),
  ServiceMessage(..),
  Message(..)
  ) where

import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import qualified Linear as L
import Graphics.Rendering.OpenGL (GLfloat)

import GCommon.Types.Generic (PlayerSettings)



data ConnectionMessage = ConnectionRequest PlayerSettings
                       | ConnectionTerminated
                       deriving (Show, Eq, Generic)

data PingMessage = PingRequest
                 deriving (Show, Eq, Generic)

data WorldMessage = PositionUpdate (L.V2 GLfloat, GLfloat)
                  | SettingsUpdate PlayerSettings
                  | Fire
                  deriving (Show, Eq, Generic)

data ServiceMessage = PingMessage PingMessage
                    | ConnectionMessage ConnectionMessage
                    deriving (Show, Eq, Generic)

data Message = WorldMessage WorldMessage
             | ServiceMessage ServiceMessage
             deriving (Show, Eq, Generic)

instance Serialize ConnectionMessage
instance Serialize PingMessage
instance Serialize WorldMessage
instance Serialize ServiceMessage
instance Serialize Message