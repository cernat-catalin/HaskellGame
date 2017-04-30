{-# LANGUAGE RecordWildCards #-}

module GMainLoop.Server (
  mainLoop
  ) where

import Control.Monad (join, unless)
import Control.Monad.State (execState)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, isEmptyTChan, readTChan)
import Text.Printf (printf)

import GState.Server (Server(..))
import GNetwork.Server (broadcast)
import GMessages.Common (Message(..), WorldMessage(..))
import GMessages.Server (KeyWorldMessage(..))
import Common.GObjects (World(..), WorldS)
import Common.GTransform (getPlayer, movePlayerLeft, movePlayerRight, movePlayerUp, movePlayerDown, updatePlayer, addPlayer, removePlayer)
import GLogger.Server (logInfo)


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

processMessage :: KeyWorldMessage -> WorldS ()
processMessage (KeyWorldMessage key message) =
  case message of
    MoveLeft   -> updatePlayer key movePlayerLeft
    MoveRight  -> updatePlayer key movePlayerRight
    MoveUp     -> updatePlayer key movePlayerUp
    MoveDown   -> updatePlayer key movePlayerDown
    AddPlayer  -> addPlayer key
    RemovePlayer -> removePlayer key
    _          -> pure ()

sendUpdates :: Server -> IO ()
sendUpdates server@Server{..} = atomically $ broadcast server (WorldMessage $ WorldUpdate world)