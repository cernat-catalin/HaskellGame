{-# LANGUAGE RecordWildCards #-}

module GCommon.Objects.Transforms (
  addPlayer,
  changePlayerSettings,
  removePlayer,
  getPlayer,
  updatePlayer,
  moveVehicle,
  setOrientation,
  addBullet,
  addBulletsFromPlayer
  ) where

import Control.Monad.State (execState, modify, get, put, StateT(..))
import Data.Functor.Identity (Identity)
import Control.Monad.Trans (lift)
import qualified Data.Map as Map
import Data.Map (insert)
import Control.Lens ((^.), uses, (%=), (.=), use, view, zoom, set)
import qualified Linear as L
import Linear (V2(..))
import Graphics.Rendering.OpenGL (GLfloat)

import GCommon.Objects.Objects as GO
import GCommon.Types.Generic (ClientKey, PlayerSettings)
import GInput.Client (Direction(..))
import GCommon.Geometry (Angle, translate, rotate, scale)



addPlayer :: Player -> WorldS ()
addPlayer ply = players %= (Map.insert (ply ^. pClientKey) ply)

changePlayerSettings :: ClientKey -> PlayerSettings -> WorldS ()
changePlayerSettings key settings = getPlayer key >>=
  \plyM -> case plyM of
    Nothing  -> return ()
    Just ply -> do
      players %= (Map.insert (ply ^. pClientKey)) (newPlayer key settings)

removePlayer :: ClientKey -> WorldS ()
removePlayer key = players %= (Map.delete key)

getPlayer :: ClientKey -> WorldS (Maybe Player)
getPlayer key = uses players (Map.lookup key)

updatePlayer :: ClientKey -> PlayerS () -> WorldS ()
updatePlayer key comp = getPlayer key >>=
  \plyM -> case plyM of
    Nothing  -> return ()
    Just ply -> players %= (Map.insert (ply ^. pClientKey)) (execState comp ply)


moveVehicle :: Direction -> VehicleS ()
moveVehicle dir = use vSpeed >>= \s -> let (x, y) = help dir s in vPosition %= (L.^+^ (L.V2 x y))
  where
    help dir' s' = case dir' of
        DUp    -> (0.0, s')
        DLeft  -> (-s', 0.0)
        DDown  -> (0.0, -s')
        DRight -> (s', 0.0)

setOrientation :: Angle -> VehicleS ()
setOrientation angle = vOrientation .= angle

addBullet :: Bullet -> WorldS ()
addBullet bullet = bullets %= (bullet :)

upVectorDim :: L.V2 GLfloat -> L.V3 GLfloat
upVectorDim (L.V2 x y) = L.V3 x y 1

downVectorDim :: L.V3 GLfloat -> L.V2 GLfloat
downVectorDim (L.V3 x y z) = L.V2 x y

addBulletsFromPlayer :: ClientKey -> WorldS ()
addBulletsFromPlayer key = getPlayer key >>=
  \plyM -> case plyM of
    Nothing  -> return ()
    Just ply -> do
      let vehicleTrans = rotate $ ply ^. pVehicle . vOrientation - (pi / 2)
      foldr (>>) (return ()) $ map (\w -> do
        let 
          bulletPos   =  (ply ^. pVehicle . vPosition) L.^+^ (downVectorDim $ vehicleTrans L.!* (upVectorDim (w ^. wPosition)))
          bulletAngle = (ply ^. pVehicle . vOrientation + w ^. wOrientation)
          bullet      = (w ^. wBullet) {_bPosition = bulletPos, _bOrientation = bulletAngle}
        addBullet $ bullet) (ply ^. pVehicle . vWeapons)