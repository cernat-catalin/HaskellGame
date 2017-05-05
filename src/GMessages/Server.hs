module GMessages.Server (
  ConnectionMessage(..),
  PingMessage(..),
  WorldMessage(..),
  ServiceMessage(..),
  KeyMessage(..)
  ) where

import Common.GTypes (ClientKey, Point, ClientSettings(..))



data ConnectionMessage = ConnectionRequest ClientSettings
                       | ConnectionTerminated
                       deriving (Show, Eq)

data PingMessage = PingRequest
                 deriving (Show, Eq)

data WorldMessage = PositionUpdate Point
                  | AddPlayer
                  | RemovePlayer

data ServiceMessage = PingMessage PingMessage
                    deriving (Show, Eq)

data KeyMessage a = KeyMessage ClientKey a