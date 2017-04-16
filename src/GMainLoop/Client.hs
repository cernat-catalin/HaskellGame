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
import Text.Printf (printf)

import Common.GObjects (World(..), WorldS)
import Common.GTypes (Message(..))
import GState.Client (ClientState(..))
import GLogger.Client (logInfo)
import GOpenGL.Client (drawWorld)
import GInput.Client (processInputMessages)


mainLoop :: ClientState -> GLFW.Window -> IO ()
mainLoop clientState@ClientState{..} window = do
  threadDelay 14000 -- 0.014 sec

  processInputMessages clientState
  world' <- updateWorld clientState
  let clientState' = clientState { world = world' }
  drawWorld clientState' window
  shouldQuit_ <- atomically $ readTVar shouldQuit
  unless shouldQuit_ $ do
    mainLoop clientState' window

updateWorld :: ClientState -> IO World
updateWorld clientState@ClientState{..} = join $ atomically $ do
  emptyChan <- isEmptyTChan worldMessages
  if not emptyChan
    then do
        message <- readTChan worldMessages
        return $ do
          let world' = execState (processMessage message) world
          updateWorld clientState { world = world' }
    else do
      return $ pure world

processMessage :: Message -> WorldS ()
processMessage message =
  case message of
    WorldUpdate world -> modify (\_ -> world)