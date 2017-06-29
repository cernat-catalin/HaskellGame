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
import Data.Time.Clock (UTCTime)




data ConnectionMessage = PlayerKey ClientKey
                       deriving (Show, Eq)

data PingMessage = PingRequest
                 | PingResponse UTCTime
                 deriving (Show, Eq)

data WorldMessage = WorldUpdate World
                  deriving (Show, Eq)

data WorldInputMessage = PressUp    | ReleaseUp
                       | PressLeft  | ReleaseLeft
                       | PressDown  | ReleaseDown
                       | PressRight | ReleaseRight
                       | PressSpace | ReleaseSpace
                       deriving (Show, Eq)

data ServiceMessage = PingMessage PingMessage
                    deriving (Show, Eq)

data SettingsMessage = Quit
                     | OpenMenu
                     deriving (Show, Eq)