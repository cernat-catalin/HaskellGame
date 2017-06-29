{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module GCommon.Objects.Objects (
  Weapon(..), WeaponsS, wPosition, wOrientation, wBullet,
  Vehicle(..), VehicleS, vPosition, vOrientation, vMeshId, vWeapons, vBounding, vHealth, vSpeed, vFireRate,
  Bullet(..), BulletS, bPosition, bOrientation, bBounding, bTeam, bSpeed, bTravel, bDamage, bMeshId,
  Player(..), PlayerS, pClientKey, pVehicle, pName, pTeam, pScore, newPlayer, playerReset, pDead,
  World(..), WorldS, players, bullets, newWorld, wBounding
  ) where


import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Control.DeepSeq (NFData)
import Network.Socket (SockAddr(..), PortNumber(..))
import Control.Monad.State (State)
import Control.Lens (makeLenses)
import qualified Data.Map as Map
import Linear (V2(..))
import Graphics.Rendering.OpenGL (GLfloat)

import GCommon.Types.Generic (ClientKey, PlayerSettings(..), PlayerSettingsReset(..))
import GCommon.Geometry (Rectangle(..), Angle)



data Bullet = Bullet {
  _bPosition    :: V2 GLfloat,
  _bOrientation :: Angle,
  _bMeshId      :: Int,
  _bBounding    :: Rectangle,
  _bTeam        :: Int,
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
  _vFireRate    :: Double,
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
  _pScore     :: Int,
  _pDead      :: Bool
} deriving (Show, Eq, Generic)

type PlayerS = State Player
makeLenses ''Player


data World = World {
  _players :: Map.Map ClientKey Player,
  _bullets :: [Bullet],
  _wBounding :: Rectangle
} deriving (Show, Eq, Generic)

type WorldS = State World
makeLenses ''World


instance NFData Bullet
instance NFData Weapon
instance NFData Vehicle
instance NFData PortNumber
instance NFData SockAddr
instance NFData Player

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
  _vMeshId    = 1,
  _vBounding  = Rectangle 0 0 0 0,
  _vFireRate = 200,

  _vWeapons = [
      (Weapon (V2 0 0.20) 0 bullet1),
      (Weapon (V2 0.11 0.12) ((-pi) / 2) bullet1),
      (Weapon (V2 (-0.11) 0.12) (pi / 2) bullet1)
  ],

  _vHealth    = 100,
  _vSpeed     = 0.015
}

bullet1 :: Bullet
bullet1 = Bullet {
  _bPosition = V2 0 0,
  _bBounding = Rectangle (-0.3) 0.3 0.3 (-0.3),
  _bOrientation = 0,
  _bMeshId = 1,
  _bTeam = 0,
  _bSpeed = 1,
  _bTravel = 5,
  _bDamage = 5
}

vehicle2 :: Vehicle
vehicle2 = Vehicle {
  _vPosition = V2 0 0,
  _vOrientation = 0,
  _vMeshId    = 2,
  _vBounding  = Rectangle 0 0 0 0,
  _vFireRate = 1000,

  _vWeapons = [(Weapon (V2 0 0.40) 0 bullet2)],

  _vHealth    = 100,
  _vSpeed     = 0.0075
}

bullet2 :: Bullet
bullet2 = Bullet {
  _bPosition = V2 0 0,
  _bBounding = Rectangle (-0.3) 0.3 0.3 (-0.3),
  _bOrientation = 0,
  _bMeshId = 2,
  _bTeam = 0,
  _bSpeed = 0.3,
  _bTravel = 5,
  _bDamage = 10
}

vehicle3 :: Vehicle
vehicle3 = Vehicle {
  _vPosition = V2 0 0,
  _vOrientation = 0,
  _vMeshId    = 3,
  _vBounding  = Rectangle 0 0 0 0,
  _vFireRate = 500,

  _vWeapons = [
    (Weapon (V2 (-0.0875) 0.3) 0 bullet3),
    (Weapon (V2 (0.0875) 0.3) 0 bullet3)
  ],

  _vHealth    = 100,
  _vSpeed     = 0.01
}

bullet3 :: Bullet
bullet3 = Bullet {
  _bPosition = V2 0 0,
  _bBounding = Rectangle (-0.3) 0.3 0.3 (-0.3),
  _bOrientation = 0,
  _bMeshId = 3,
  _bTeam = 0,
  _bSpeed = 0.7,
  _bTravel = 5,
  _bDamage = 10
}



newVehicle :: Int -> Vehicle
newVehicle vehicleId = case vehicleId of
  1 -> vehicle1
  3 -> vehicle2
  2 -> vehicle3

newPlayer :: ClientKey -> PlayerSettings -> Player
newPlayer key PlayerSettings{..} = Player {
  _pClientKey = key,
  _pVehicle   = newVehicle vehicleId,
  _pName      = name,
  _pTeam      = team,
  _pScore     = 0,
  _pDead      = False
}

playerReset :: ClientKey -> String -> PlayerSettingsReset -> Player
playerReset key name' PlayerSettingsReset{..} = Player {
  _pClientKey = key,
  _pVehicle   = newVehicle rVehicleId,
  _pName      = name',
  _pTeam      = rTeam,
  _pScore     = 0,
  _pDead      = False
}

newWorld :: World
newWorld = World {
  _players = Map.empty,
  _bullets = [],
  _wBounding = Rectangle (-5) 5 5 (-5)
}