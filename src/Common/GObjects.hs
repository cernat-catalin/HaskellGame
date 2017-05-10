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

import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Network.Socket (SockAddr(..), PortNumber(..))
import Control.Monad.State (State)
import qualified Data.Map as Map

import Common.GTypes (ClientKey, Point, Radius)


data Circle = Circle {
  center :: Point,
  radius :: Radius
} deriving (Show, Eq, Generic)

type CircleS = State Circle

data Player = Player {
  clientKey :: ClientKey,
  circle    :: Circle
} deriving (Show, Eq, Generic)

type PlayerS = State Player

data World = World {
  players :: Map.Map ClientKey Player
} deriving (Show, Eq, Generic)

type WorldS = State World

newWorld :: World
newWorld = World {
  players = Map.empty
}

instance Serialize Circle
deriving instance Generic SockAddr
deriving instance Generic PortNumber
instance Serialize PortNumber
instance Serialize SockAddr
instance Serialize Player
instance Serialize World