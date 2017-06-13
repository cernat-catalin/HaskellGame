module GMessages.Client (
  ConnectionMessage(..),
  PingMessage(..),
  WorldMessage(..),
  WorldInputMessage(..),
  ServiceMessage(..),
  SettingsMessage(..)
  ) where

import GCommon.Objects.Objects (World)
import GCommon.Types.Generic (ClientKey)




data ConnectionMessage = PlayerKey ClientKey
                       deriving (Show, Eq)

data PingMessage = PingRequest
                 | PingResponse String
                 deriving (Show, Eq)

data WorldMessage = WorldUpdate World
                  deriving (Show, Eq)

data WorldInputMessage = PressUp    | ReleaseUp
                       | PressLeft  | ReleaseLeft
                       | PressDown  | ReleaseDown
                       | PressRight | ReleaseRight
                       deriving (Show, Eq)

data ServiceMessage = PingMessage PingMessage
                    deriving (Show, Eq)

data SettingsMessage = Quit
                     deriving (Show, Eq)