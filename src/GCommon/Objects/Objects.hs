{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module GCommon.Objects.Objects (
  Weapon(..), WeaponsS, wPosition, wOrientation, wBullet,
  Vehicle(..), VehicleS, vPosition, vOrientation, vMeshId, vWeapons, vBounding, vHealth, vSpeed,
  Bullet(..), BulletS, bPosition, bOrientation, bSpeed, bTravel, bDamage,
  Player(..), PlayerS, pClientKey, pVehicle, pName, pTeam, pScore, newPlayer,
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

import GCommon.Types.Generic (ClientKey, PlayerSettings(..))
import GCommon.Geometry (Rectangle(..), Angle)



data Bullet = Bullet {
  _bPosition    :: V2 GLfloat,
  _bOrientation :: Angle,
  _bSpeed       :: GLfloat,
  _bTravel      :: GLfloat,
  _bDamage      :: GLfloat
} deriving (Show, Eq, Generic)

type BulletS = State Bullet
makeLenses ''Bullet


data Weapon = Weapon {
  _wPosition    :: V2 GLfloat,
  _wOrientation :: Angle,
  _wBullet      :: Bullet
} deriving (Show, Eq, Generic)

type WeaponsS = State Weapon
makeLenses ''Weapon


data Vehicle = Vehicle {
  _vPosition    :: V2 GLfloat,
  _vOrientation :: Angle,
  _vMeshId      :: Int,
  _vBounding    :: Rectangle,
  _vWeapons     :: [Weapon],
  _vHealth      :: Float,
  _vSpeed       :: Float
} deriving (Show, Eq, Generic)

type VehicleS = State Vehicle
makeLenses ''Vehicle

data Player = Player {
  _pClientKey :: ClientKey,
  _pVehicle   :: Vehicle,
  _pName      :: String,
  _pTeam      :: Int,
  _pScore     :: Int
} deriving (Show, Eq, Generic)

type PlayerS = State Player
makeLenses ''Player


data World = World {
  _players :: Map.Map ClientKey Player,
  _bullets :: [Bullet]
} deriving (Show, Eq, Generic)

type WorldS = State World
makeLenses ''World


deriving instance Generic SockAddr
deriving instance Generic PortNumber
instance Serialize PortNumber
instance Serialize SockAddr
instance Serialize Weapon
instance Serialize Vehicle
instance Serialize Bullet
instance Serialize Player
instance Serialize World


-- Predefined objects

vehicle1 :: Vehicle
vehicle1 = Vehicle {
  _vPosition = V2 0 0,
  _vOrientation = 0,
  _vMeshId    = 2,
  _vBounding  = Rectangle 0 0 0 0,

  _vWeapons = [
      (Weapon (V2 0 0.20) 0 bullet1),
      (Weapon (V2 0.11 0.12) ((-pi) / 2) bullet1),
      (Weapon (V2 (-0.11) 0.12) (pi / 2) bullet1)
  ],

  _vHealth    = 0,
  _vSpeed     = 0.01
}

bullet1 :: Bullet
bullet1 = Bullet {
  _bPosition = V2 0 0,
  _bOrientation = 0,
  _bSpeed = 0.5,
  _bTravel = 5,
  _bDamage = 10
}


newVehicle :: Int -> Vehicle
newVehicle vehicleId = case vehicleId of
  1 -> vehicle1

newPlayer :: ClientKey -> PlayerSettings -> Player
newPlayer key PlayerSettings{..} = Player {
  _pClientKey = key,
  _pVehicle   = newVehicle vehicleId,
  _pName      = name,
  _pTeam      = team,
  _pScore     = 0
}

newWorld :: World
newWorld = World {
  _players = Map.empty,
  _bullets = []
}