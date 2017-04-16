{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Common.GObjects (
  Point,
  Radius,
  Circle(..),
  CircleS,
  Player(..),
  PlayerS,
  World(..),
  WorldS,
  newWorld
  ) where

import Graphics.Rendering.OpenGL (GLdouble)
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Network.Socket (SockAddr(..), PortNumber(..))
import Control.Monad.State (State)
import qualified Data.Map as Map


type Point = (GLdouble, GLdouble)

type Radius = GLdouble

data Circle = Circle {
  center :: Point,
  radius :: Radius
} deriving (Show, Generic, Eq)

type CircleS = State Circle

instance Serialize Circle

data Player = Player {
  clientAddr :: SockAddr,
  circle     :: Circle
} deriving (Show, Generic, Eq)

type PlayerS = State Player

-- TODO Now we get an 'Orphan instace' warning. Generate Serialize instance for SockAddr and PortNumber or surpress warning
deriving instance Generic SockAddr

deriving instance Generic PortNumber

instance Serialize PortNumber

instance Serialize SockAddr

instance Serialize Player

data World = World {
  players :: Map.Map SockAddr Player
} deriving (Show, Generic, Eq)

type WorldS = State World

instance Serialize World

newWorld :: World
newWorld = World {
  players = Map.empty
}