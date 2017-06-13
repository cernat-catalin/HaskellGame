module GCommon.Objects.Transforms (
  addPlayer,
  removePlayer,
  getPlayer,
  updatePlayer,
  moveVehicle,
  setOrientation,
  addBullet,
  addBulletsFromPlayer
  ) where

import Control.Monad.State (execState, modify, get)
import qualified Data.Map as Map
import Control.Lens ((^.), uses, (%=), (.=), use, view, zoom)
import qualified Linear as L

import GCommon.Objects.Objects as GO
import GCommon.Types.Generic (ClientKey)
import GInput.Client (Direction(..))
import GCommon.Geometry (Angle, translate, rotate, scale)


addPlayer :: Player -> WorldS ()
addPlayer ply = players %= (Map.insert (ply ^. clientKey) ply)

removePlayer :: ClientKey -> WorldS ()
removePlayer key = players %= (Map.delete key)

getPlayer :: ClientKey -> WorldS (Maybe Player)
getPlayer key = uses players (Map.lookup key)

updatePlayer :: ClientKey -> PlayerS () -> WorldS ()
updatePlayer key comp = getPlayer key >>=
  \plyM -> case plyM of
    Nothing  -> return ()
    Just ply -> players %= (Map.insert (ply ^. clientKey)) (execState comp ply)


moveVehicle :: Direction -> VehicleS ()
moveVehicle dir = use speed >>= \s -> let (x, y) = help dir s in position %= (L.^+^ (L.V2 x y))
  where
    help dir' s' = case dir' of
        DUp    -> (0.0, s')
        DLeft  -> (-s', 0.0)
        DDown  -> (0.0, -s')
        DRight -> (s', 0.0)

setOrientation :: Angle -> VehicleS ()
setOrientation angle = orientation .= angle

addBullet :: Bullet -> WorldS ()
addBullet bullet = bullets %= (bullet :)

addBulletsFromPlayer :: ClientKey -> WorldS()
addBulletsFromPlayer key = getPlayer key >>=
  \plyM -> case plyM of
    Nothing  -> return ()
    Just ply -> foldr (>>) (return ()) $ map (\w -> addBullet $ newBullet (ply ^. vehicle . position L.^+^ w ^. wPosition) (ply ^. vehicle . orientation + w ^. wOrientation)) (ply ^. vehicle . vWeapons)