{-# LANGUAGE RecordWildCards #-}

module GMainLoop.Client (
  mainLoop  
) where

import qualified Graphics.UI.GLFW as GLFW
import Control.Monad (join, unless)
import Control.Monad.State (put, evalState, execState, modify)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, isEmptyTChan, readTChan)
import Control.Concurrent.STM.TVar (readTVar)
import Text.Printf (printf)

import Common.GObjects (World(..), WorldS, Player(..), Circle(..))
import GState.Client (ClientState(..))
import GOpenGL.Client (drawWorld)
import Common.GTransform (getPlayer, updatePlayer, movePlayerLeft, movePlayerRight, movePlayerUp, movePlayerDown)
import GLogger.Client (logInfo)
import GNetwork.Client (sendMessage)
import qualified GMessages.Client as C
import qualified GMessages.Network.ClientServer as CS



mainLoop :: ClientState -> GLFW.Window -> IO ()
mainLoop clientState@ClientState{..} window = do
  threadDelay 14000 -- 0.014 sec

  -- Process input messages and send new position
  world' <- processWorldInput clientState
  let clientState' = clientState { world = world' }
  sendPositionUpdate clientState

  -- Process server world updates
  world'' <- updateWorld clientState'
  let clientState'' = clientState' { world = world'' }

  -- Draw world
  drawWorld clientState'' window
  shouldQuit_ <- atomically $ readTVar shouldQuit
  unless shouldQuit_ $ do
    mainLoop clientState'' window

updateWorld :: ClientState -> IO World
updateWorld clientState@ClientState{..} = join $ atomically $ do
  emptyChan <- isEmptyTChan worldUpdateChan 
  if not emptyChan
    then do
        message <- readTChan worldUpdateChan
        return $ do
          let playerM = evalState (getPlayer playerKey) world
              updateFunc = case playerM of
                Nothing -> return ()
                Just player -> put player
              world' = execState (processWorldMessage message >> updatePlayer playerKey updateFunc) world
          updateWorld clientState { world = world' }
    else do
      return $ pure world

processWorldMessage :: C.WorldMessage -> WorldS ()
processWorldMessage message =
  case message of
    C.WorldUpdate world -> modify (\_ -> world)

processWorldInput :: ClientState -> IO World
processWorldInput clientState@ClientState{..} = join $ atomically $ do
  emptyChan <- isEmptyTChan worldInputChan
  if not emptyChan
    then do
        message <- readTChan worldInputChan
        return $ do
          let world' = execState (processInputMessage clientState message) world
          processWorldInput clientState { world = world' }
    else do
      return $ pure world

processInputMessage :: ClientState -> C.WorldInputMessage -> WorldS ()
processInputMessage ClientState{..} message =
  case message of
    C.MoveLeft  -> updatePlayer playerKey movePlayerLeft
    C.MoveRight -> updatePlayer playerKey movePlayerRight
    C.MoveUp    -> updatePlayer playerKey movePlayerUp
    C.MoveDown  -> updatePlayer playerKey movePlayerDown

sendPositionUpdate :: ClientState -> IO ()
sendPositionUpdate ClientState{..} = do
  let playerM = evalState (getPlayer playerKey) world
  case playerM of
    Just player -> do
      _ <- sendMessage serverHandle (CS.WorldMessage $ CS.PositionUpdate (center $ circle player))
      return ()
    Nothing     -> return ()