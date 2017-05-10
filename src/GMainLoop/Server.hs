{-# LANGUAGE RecordWildCards #-}

module GMainLoop.Server (
  mainLoop
  ) where

import Control.Monad (join)
import Control.Monad.State (execState)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, isEmptyTChan, readTChan)

import GState.Server (Server(..))
import GNetwork.Server (broadcast)
import GMessages.Server as S (KeyMessage(..), WorldMessage(..))
import GMessages.Network.ServerClient as SC (Message(..), WorldMessage(..))
import Common.GObjects (World(..), WorldS)
import Common.GTransform (updatePlayer, movePlayer, addPlayer, removePlayer)


mainLoop :: Server -> IO ()
mainLoop server@Server{..} = do
--   logInfo (printf "Tick tock")
  world' <- updateWorld server
  let server' = server { world = world' }

  sendUpdates server'
  threadDelay 14000 --0.014

  mainLoop server'

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
    AddPlayer -> addPlayer key
    RemovePlayer -> removePlayer key
    PositionUpdate position -> updatePlayer key (movePlayer position)

sendUpdates :: Server -> IO ()
sendUpdates server@Server{..} = atomically $ broadcast server (SC.WorldMessage $ SC.WorldUpdate world)