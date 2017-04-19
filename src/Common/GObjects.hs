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

instance Serialize Circle

data Player = Player {
  clientKey :: ClientKey,
  circle    :: Circle
} deriving (Show, Eq, Generic)

type PlayerS = State Player

-- TODO Now we get an 'Orphan instace' warning. Generate Serialize instance for SockAddr and PortNumber or surpress warning
deriving instance Generic SockAddr

deriving instance Generic PortNumber

instance Serialize PortNumber

instance Serialize SockAddr

instance Serialize Player

data World = World {
  players :: Map.Map ClientKey Player
} deriving (Show, Eq, Generic)

type WorldS = State World

instance Serialize World

newWorld :: World
newWorld = World {
  players = Map.empty
}