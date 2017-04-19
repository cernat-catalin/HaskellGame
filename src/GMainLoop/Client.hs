{-# LANGUAGE RecordWildCards #-}

module GMainLoop.Client (
  mainLoop  
) where

import qualified Graphics.UI.GLFW as GLFW
import Control.Monad (join, unless)
import Control.Monad.State (execState, modify)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, isEmptyTChan, readTChan)
import Control.Concurrent.STM.TVar (readTVar)

import Common.GObjects (World(..), WorldS)
import Common.GMessages (WorldMessage(..))
import GState.Client (ClientState(..))
import GOpenGL.Client (drawWorld)
import GInput.Client (processWorldInput)


mainLoop :: ClientState -> GLFW.Window -> IO ()
mainLoop clientState@ClientState{..} window = do
  threadDelay 14000 -- 0.014 sec

  processWorldInput clientState
  world' <- updateWorld clientState
  let clientState' = clientState { world = world' }
  drawWorld clientState' window
  shouldQuit_ <- atomically $ readTVar shouldQuit
  unless shouldQuit_ $ do
    mainLoop clientState' window

updateWorld :: ClientState -> IO World
updateWorld clientState@ClientState{..} = join $ atomically $ do
  emptyChan <- isEmptyTChan worldUpdateChan 
  if not emptyChan
    then do
        message <- readTChan worldUpdateChan
        return $ do
          let world' = execState (processWorldMessage message) world
          updateWorld clientState { world = world' }
    else do
      return $ pure world

processWorldMessage :: WorldMessage -> WorldS ()
processWorldMessage message =
  case message of
    WorldUpdate world -> modify (\_ -> world)