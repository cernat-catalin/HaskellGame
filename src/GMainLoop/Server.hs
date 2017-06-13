{-# LANGUAGE RecordWildCards #-}

module GMainLoop.Server (
  mainLoop
  ) where

import Control.Monad (join)
import Control.Monad.State (execState)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, isEmptyTChan, readTChan)
import Control.Lens ((.=), (^.))
import qualified Linear as L
import Data.Maybe (catMaybes)

import GState.Server (Server(..))
import GNetwork.Server (broadcast)
import GMessages.Server as S (KeyMessage(..), WorldMessage(..))
import GMessages.Network.ServerClient as SC (Message(..), WorldMessage(..))
import GCommon.Objects.Objects as GO
import GCommon.Objects.Transforms (updatePlayer, addPlayer, removePlayer, addBulletsFromPlayer)


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
    AddPlayer -> addPlayer (newPlayer key)
    RemovePlayer -> removePlayer key
    PositionUpdate (pos, angle)-> updatePlayer key (((vehicle . position) .= pos) >> ((vehicle. orientation) .= angle))
    Fire -> addBulletsFromPlayer key

simulateWorld :: Server -> IO World
simulateWorld Server{..} = do
  let bullets' = catMaybes $ map moveBullet (world ^. bullets)
  return $ world { _bullets = bullets' }


moveBullet :: Bullet -> Maybe Bullet
moveBullet bullet@Bullet{..} =
  let d = _bspeed / 60
      travel' = _btravel - d
      (L.V2 x y) = _bposition
      (x', y') = (d * cos _borientation + x, d * sin _borientation + y)
  in if (travel' <= 0) then Nothing else Just $ bullet { _btravel = travel', _bposition = L.V2 x' y' }

sendUpdates :: Server -> IO ()
sendUpdates server@Server{..} = atomically $ broadcast server (SC.WorldMessage $ SC.WorldUpdate world)