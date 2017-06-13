{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module GCommon.Objects.Objects (
  Weapon(..), WeaponsS, wPosition, wOrientation,
  Vehicle(..), VehicleS, position, orientation, meshId, vWeapons, bounding, health, speed, damage,
  Bullet(..), BulletS, bposition, borientation, bspeed, btravel, newBullet,
  Player(..), PlayerS, clientKey, vehicle, newPlayer,
  World(..), WorldS, players, bullets, newWorld
  ) where

import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Network.Socket (SockAddr(..), PortNumber(..))
import Control.Monad.State (State)
import Control.Lens (makeLenses)
import qualified Data.Map as Map
import Linear (V2(..))
import Graphics.Rendering.OpenGL (GLfloat)

import GCommon.Types.Generic (ClientKey)
import GCommon.Geometry (Rectangle(..), Angle)


data Weapon = Weapon {
  _wPosition :: V2 GLfloat,
  _wOrientation :: Angle
} deriving (Show, Eq, Generic)

type WeaponsS = State Weapon
makeLenses ''Weapon


data Vehicle = Vehicle {
  _position  :: V2 GLfloat,
  _orientation :: Angle,
  _meshId    :: Int,
  _bounding  :: Rectangle,

  _vWeapons  :: [Weapon],

  _health    :: Float,
  _speed     :: Float,
  _damage    :: Float
} deriving (Show, Eq, Generic)

type VehicleS = State Vehicle
makeLenses ''Vehicle

newVehicle :: Vehicle
newVehicle = Vehicle {
  _position = V2 0 0,
  _orientation = 0,
  _meshId    = 2,
  _bounding  = Rectangle 0 0 0 0,

  _vWeapons = [
      (Weapon (V2 0 0.20) 0),
      (Weapon (V2 0.11 0.12) ((-pi) / 2)),
      (Weapon (V2 (-0.11) 0.12) (pi / 2))
  ],

  _health    = 0,
  _speed     = 0.01,
  _damage    = 0
}


data Bullet = Bullet {
  _bposition :: V2 GLfloat,
  _borientation :: Angle,
  _bspeed       :: GLfloat,
  _btravel      :: GLfloat
} deriving (Show, Eq, Generic)

type BulletS = State Bullet
makeLenses ''Bullet

newBullet :: V2 GLfloat -> Angle -> Bullet
newBullet position@(V2 x y) angle = Bullet {
  _bposition = position,
  _borientation = angle,
  _bspeed = 0.5,
  _btravel = 0.7
}


data Player = Player {
  _clientKey :: ClientKey,
  _vehicle   :: Vehicle
} deriving (Show, Eq, Generic)

type PlayerS = State Player
makeLenses ''Player

newPlayer :: ClientKey -> Player
newPlayer key = Player {
  _clientKey = key,
  _vehicle   = newVehicle
}



data World = World {
  _players :: Map.Map ClientKey Player,
  _bullets :: [Bullet]
} deriving (Show, Eq, Generic)

type WorldS = State World
makeLenses ''World

newWorld :: World
newWorld = World {
  _players = Map.empty,
  _bullets = []
}



deriving instance Generic SockAddr
deriving instance Generic PortNumber
instance Serialize PortNumber
instance Serialize SockAddr
instance Serialize Weapon
instance Serialize Vehicle
instance Serialize Bullet
instance Serialize Player
instance Serialize World