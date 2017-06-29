module GMessages.Server (
  ConnectionMessage(..),
  PingMessage(..),
  WorldMessage(..),
  ServiceMessage(..),
  KeyMessage(..)
  ) where


import qualified Linear as L
import Graphics.Rendering.OpenGL (GLfloat)
import Data.Time.Clock (UTCTime)

import GCommon.Types.Generic (ClientKey, PlayerSettings(..), PlayerSettingsReset)
import GCommon.Geometry (Point)



data ConnectionMessage = ConnectionRequest PlayerSettings
                       | ConnectionTerminated
                       deriving (Show, Eq)

data PingMessage = PingRequest UTCTime
                 deriving (Show, Eq)

data WorldMessage = PositionUpdate (L.V2 GLfloat, GLfloat)
                  | SettingsUpdate PlayerSettings
                  | SettingsReset PlayerSettingsReset
                  | AddPlayer PlayerSettings
                  | RemovePlayer
                  | Fire

data ServiceMessage = PingMessage PingMessage
                    | ConnectionMessage ConnectionMessage
                    deriving (Show, Eq)

data KeyMessage a = KeyMessage ClientKey a