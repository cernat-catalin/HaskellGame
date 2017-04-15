{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Common.GObjects (
  Point,
  Radius,
  Circle(..),
  Player(..),
  World(..)
  ) where

import Graphics.Rendering.OpenGL (GLdouble)
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Network.Socket (SockAddr(..), PortNumber(..))


type Point = (GLdouble, GLdouble)

type Radius = GLdouble

data Circle = Circle {
  center :: Point,
  radius :: Radius
} deriving (Show, Generic)

instance Serialize Circle

data Player = Player {
  clientAddr :: SockAddr,
  circle     :: Circle
} deriving (Show, Generic)

-- TODO Now we get an 'Orphan instace' warning. Generate Serialize instance for SockAddr and PortNumber or surpress warning
deriving instance Generic SockAddr

deriving instance Generic PortNumber

instance Serialize PortNumber

instance Serialize SockAddr

instance Serialize Player

data World = World {
  players :: [Player]
} deriving (Show, Generic)

instance Serialize World