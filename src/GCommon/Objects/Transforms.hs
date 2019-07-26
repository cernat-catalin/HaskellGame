{-# LANGUAGE RecordWildCards #-}

module GCommon.Objects.Transforms
  ( addPlayer
  , changePlayerSettings
  , changePlayerSettingsReset
  , removePlayer
  , getPlayer
  , updatePlayer
  , moveVehicle
  , setOrientation
  , addBullet
  , addBulletsFromPlayer
  , addToHealth
  )
where

import           Control.Monad.State            ( execState
                                                , modify
                                                , get
                                                , put
                                                , StateT(..)
                                                )
import           Data.Functor.Identity          ( Identity )
import           Control.Monad.Trans            ( lift )
import qualified Data.Map                      as Map
import           Data.Map                       ( insert )
import           Control.Lens                   ( (^.)
                                                , uses
                                                , (%=)
                                                , (.=)
                                                , use
                                                , view
                                                , zoom
                                                , set
                                                )
import qualified Linear                        as L
import           Linear                         ( V2(..) )
import           Graphics.Rendering.OpenGL      ( GLfloat )

import           GCommon.Objects.Objects       as GO
import           GCommon.Types.Generic          ( ClientKey
                                                , PlayerSettings
                                                , PlayerSettingsReset
                                                , Direction(..)
                                                )
import           GCommon.Geometry               ( Angle
                                                , translate
                                                , rotate
                                                , scale
                                                , Rectangle
                                                , insideRectangle
                                                )



addPlayer :: Player -> WorldS ()
addPlayer ply = players %= (Map.insert (ply ^. pClientKey) ply)

changePlayerSettings :: ClientKey -> PlayerSettings -> WorldS ()
changePlayerSettings key settings = getPlayer key >>= \plyM -> case plyM of
  Nothing -> return ()
  Just ply ->
    players %= Map.insert (ply ^. pClientKey) (newPlayer key settings)

changePlayerSettingsReset :: ClientKey -> PlayerSettingsReset -> WorldS ()
changePlayerSettingsReset key settings = getPlayer key >>= \plyM ->
  case plyM of
    Nothing  -> return ()
    Just ply -> players %= Map.insert
      (ply ^. pClientKey)
      (playerReset key (ply ^. pName) settings)

removePlayer :: ClientKey -> WorldS ()
removePlayer key = players %= Map.delete key

getPlayer :: ClientKey -> WorldS (Maybe Player)
getPlayer key = uses players (Map.lookup key)

addToHealth :: Float -> PlayerS ()
addToHealth h = (pVehicle . vHealth) %= (max 0 . min 100 . (+ h))

updatePlayer :: ClientKey -> PlayerS () -> WorldS ()
updatePlayer key comp = getPlayer key >>= \plyM -> case plyM of
  Nothing  -> return ()
  Just ply -> players %= Map.insert (ply ^. pClientKey) (execState comp ply)


moveVehicle :: Rectangle -> Direction -> VehicleS ()
moveVehicle bounds dir = use vSpeed
  >>= \s -> use vPosition >>= \p -> vPosition %= const (newPos p s)
 where
  newPos (L.V2 x y) s =
    let (ofx, ofy) = help s
        inside     = insideRectangle bounds (L.V2 (x + ofx) (y + ofy))
    in  if inside then L.V2 (x + ofx) (y + ofy) else L.V2 x y
  help s' = case dir of
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
addBulletsFromPlayer key = getPlayer key >>= \plyM -> case plyM of
  Nothing  -> return ()
  Just ply -> do
    let vehicleTrans = rotate $ ply ^. pVehicle . vOrientation - (pi / 2)
    mapM_
      (\w -> do
        let bulletPos = (ply ^. pVehicle . vPosition) L.^+^ downVectorDim
              (vehicleTrans L.!* upVectorDim (w ^. wPosition))
            bulletAngle = ply ^. pVehicle . vOrientation + w ^. wOrientation
            bullet      = (w ^. wBullet) { _bPosition    = bulletPos
                                         , _bOrientation = bulletAngle
                                         , _bTeam        = ply ^. pTeam
                                         }
        addBullet bullet
      )
      (ply ^. pVehicle . vWeapons)
