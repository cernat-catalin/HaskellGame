module GMessages.Server (
  ConnectionMessage(..),
  PingMessage(..),
  WorldMessage(..),
  ServiceMessage(..)
  ) where

import Common.GTypes (ClientKey, Point, ClientSettings(..))



data ConnectionMessage = ConnectionRequest ClientKey ClientSettings
                       | ConnectionTerminated ClientKey
                       deriving (Show, Eq)

data PingMessage = PingRequest ClientKey
                 deriving (Show, Eq)

data WorldMessage = PositionUpdate ClientKey Point
                  | AddPlayer ClientKey
                  | RemovePlayer ClientKey

data ServiceMessage = ConnectionMessage ConnectionMessage
                    | PingMessage PingMessage
                    deriving (Show, Eq)