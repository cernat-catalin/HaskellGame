module GMessages.Server (
  ConnectionMessage(..),
  PingMessage(..),
  WorldMessage(..),
  ServiceMessage(..),
  KeyMessage(..)
  ) where


import qualified Linear as L
import Graphics.Rendering.OpenGL (GLfloat)

import GCommon.Types.Generic (ClientKey, ClientSettings(..))
import GCommon.Geometry (Point)



data ConnectionMessage = ConnectionRequest ClientSettings
                       | ConnectionTerminated
                       deriving (Show, Eq)

data PingMessage = PingRequest
                 deriving (Show, Eq)

data WorldMessage = PositionUpdate (L.V2 GLfloat, GLfloat)
                  | AddPlayer
                  | RemovePlayer
                  | Fire

data ServiceMessage = PingMessage PingMessage
                    | ConnectionMessage ConnectionMessage
                    deriving (Show, Eq)

data KeyMessage a = KeyMessage ClientKey a