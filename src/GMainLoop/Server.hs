{-# LANGUAGE RecordWildCards #-}

module GMainLoop.Server (
  mainLoop
  ) where

import Control.Monad (join)
import Control.Monad.State (execState)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, isEmptyTChan, readTChan)
import Control.Lens ((.=), (^.), use, (%=))
import qualified Linear as L
import Data.Maybe (catMaybes)
import Control.DeepSeq
import Control.Parallel.Strategies
import Data.List.Ordered (union)
import qualified Data.Map as Map
import Debug.Trace

import GState.Server (Server(..))
import GNetwork.Server (broadcast)
import GMessages.Server as S (KeyMessage(..), WorldMessage(..))
import GMessages.Network.ServerClient as SC (Message(..), WorldMessage(..))
import GCommon.Objects.Objects as GO
import GCommon.Geometry (Rectangle(..), translateRectangle)
import GCommon.Objects.Transforms (updatePlayer, addPlayer, changePlayerSettings, removePlayer, addBulletsFromPlayer, addToHealth)


mainLoop :: Server -> IO ()
mainLoop server@Server{..} = do
--   logInfo (printf "Tick tock")
  world' <- updateWorld server
  let server' = server { world = world' }

  world'' <- simulateWorld server'
  let server'' = server' { world = world'' }

  sendUpdates server''
  threadDelay 14000 --0.014

  mainLoop server''

{-
  TODO: This is a big problem.
  Must have something like this (or something similar):
   - enter function
   - Copy current messages in channel
   - Process as many as possible (due to frame rate)
-}
updateWorld :: Server -> IO World
updateWorld server@Server{..} = join $ atomically $ do
  emptyChan <- isEmptyTChan worldChan
  if not emptyChan
    then do
        message <- readTChan worldChan
        return $ do
          let world' = execState (processMessage message) world
          updateWorld server { world = world' }
    else do
      return $ pure world

processMessage :: (S.KeyMessage S.WorldMessage) -> WorldS ()
processMessage (S.KeyMessage key message) =
  case message of
    AddPlayer settings -> addPlayer (newPlayer key settings)
    SettingsUpdate settings -> changePlayerSettings key settings
    RemovePlayer -> removePlayer key
    PositionUpdate (pos, angle)-> updatePlayer key (((pVehicle . vPosition) .= pos) >> ((pVehicle. vOrientation) .= angle))
    Fire -> addBulletsFromPlayer key

simulateWorld :: Server -> IO World
simulateWorld Server{..} = do
  let bullets' = catMaybes $ map moveBullet (world ^. bullets)
  return $ execState updateWorldPar $ world { _bullets = bullets' }

moveBullet :: Bullet -> Maybe Bullet
moveBullet bullet@Bullet{..} =
  let d = _bSpeed / 60
      travel' = _bTravel - d
      (L.V2 x y) = _bPosition
      (x', y') = (d * cos _bOrientation + x, d * sin _bOrientation + y)
  in if (travel' <= 0) then Nothing else Just $ bullet { _bTravel = travel', _bPosition = L.V2 x' y' }

-- collision detection

rectanglesIntersect :: Rectangle -> Rectangle -> Bool
rectanglesIntersect (Rectangle x1 y1 x2 y2) (Rectangle x1' y1' x2' y2') = 
  (x1 < x2' && x2 > x1' && y1 > y2' && y2 < y1')

checkCollision :: Player -> Bullet -> (PlayerS (), Bool)
checkCollision ply bullet =
  let playerRec = translateRectangle (ply ^. pVehicle . vBounding) (ply ^. pVehicle . vPosition)
      bulletRec = translateRectangle (bullet ^. bBounding) (bullet ^. bPosition)
  in if (ply ^. pTeam /= bullet ^. bTeam && rectanglesIntersect playerRec bulletRec)
  then (addToHealth (-bullet ^. bDamage), True)
  else (return (), False)

getIndices :: Int -> [Bool] -> [Int]
getIndices _ [] = []
getIndices n (x:xs)
  | x == True = n : getIndices (n + 1) xs
  | x == False = getIndices (n + 1) xs

checkCollisionsOnVehicle :: Player -> [Bullet] -> (Player, [Int])
checkCollisionsOnVehicle ply bullets =
  let result = Prelude.map (checkCollision ply) bullets
      indices = getIndices 1 (Prelude.map snd result)
      trans = Prelude.foldr (>>) (return ()) (Prelude.map fst result)
  in (execState trans ply, indices)

checkCollisions :: [Bullet] -> [Player] -> [(Player, [Int])]
checkCollisions bullets plys = do
  ply <- plys
  return $ checkCollisionsOnVehicle ply bullets

checkCollisionsPar :: [Bullet] -> [Player] -> [(Player, [Int])]
checkCollisionsPar bullets plys = (checkCollisions bullets plys) `using` parList rdeepseq

dropBullets :: [Bullet] -> [Int] -> [Bullet]
dropBullets bullets indices = helper 1 bullets indices
  where
    helper _ [] _ = []
    helper _ bs [] = bs
    helper n (b:bs) is'@(i:is)
      | i == n = helper (n + 1) bs is
      | otherwise = b : helper (n + 1) bs is'

updateWorldPar :: WorldS ()
updateWorldPar = use bullets >>= \bs -> use players >>= \plys -> do
  let result = checkCollisionsPar bs (map snd $ Map.toList plys)
      indices = Prelude.foldr Data.List.Ordered.union [] $ Prelude.map snd result
      plys'   = map (\(ply, _) -> (ply ^. pClientKey, ply)) result
  players %= (const $ Map.fromList plys')
  bullets  %= (const $ dropBullets bs indices)

-- end collision detection

sendUpdates :: Server -> IO ()
sendUpdates server@Server{..} = atomically $ broadcast server (SC.WorldMessage $ SC.WorldUpdate world)