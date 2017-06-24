{-# LANGUAGE RecordWildCards #-}

module GMainLoop.Client (
  mainLoop  
) where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Control.Monad (join, unless)
import Control.Monad.State (put, evalState, execState, modify)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, isEmptyTChan, readTChan)
import Control.Concurrent.STM.TVar (readTVar)
import Control.Lens ((^.), (.=), zoom)
import Data.Maybe (fromJust)
import qualified Linear as L

import GCommon.Objects.Objects as GO
import GState.Client (ClientState(..), KeysState(..))
import GOpenGL.Client (drawWorld)
import GCommon.Objects.Transforms (getPlayer, updatePlayer, moveVehicle, setOrientation)
import GNetwork.Client (sendMessage)
import qualified GMessages.Client as C
import qualified GMessages.Network.ClientServer as CS
import GInput.Client (processWorldInput, Direction(..))
import GOpenGL.Meshes (ShaderResources)
import GLogger.Client (logInfo)
import Text.Printf (printf)



mainLoop :: ClientState -> ShaderResources -> GLFW.Window -> IO ()
mainLoop clientState@ClientState{..} shaderResources window = do

  -- logInfo (printf "World = %s" (show world))

  threadDelay 14000 -- 0.014 sec

  -- Process input messages and send new position
  menuIsOn_ <- atomically $ readTVar menuIsOn
  clientState22 <- case menuIsOn_ of
    True -> do
      _ <- processWorldInput clientState
      _ <- processMousePos clientState window
      return clientState
    False -> do
      clientState1  <- processWorldInput clientState
      let clientState21 = processKeysState clientState1
      processMousePos clientState21 window

  sendPositionUpdate clientState22

  -- Process server world updates
  world' <- serverUpdateWorld clientState22
  let clientState3 = clientState22 { world = world' }

  -- Draw world
  drawWorld clientState3 shaderResources window
  shouldQuit_ <- atomically $ readTVar shouldQuit
  unless shouldQuit_ $ do
    mainLoop clientState3 shaderResources window

processKeysState :: ClientState -> ClientState
processKeysState clientState@ClientState{..} =
  let tUp    = if up keysState then updatePlayer playerKey (zoom pVehicle (moveVehicle DUp)) else return ()
      tLeft  = if left keysState then updatePlayer playerKey (zoom pVehicle (moveVehicle DLeft)) else return ()
      tDown  = if down keysState then updatePlayer playerKey (zoom pVehicle (moveVehicle DDown)) else return ()
      tRight = if right keysState then updatePlayer playerKey (zoom pVehicle (moveVehicle DRight)) else return ()
  in clientState {world = (execState (tUp >> tLeft >> tDown >> tRight)) world }

processMousePos :: ClientState -> GLFW.Window -> IO ClientState
processMousePos clientState@ClientState{..} window = do
  (x, y) <- GLFW.getCursorPos window
  (width, height) <- GLFW.getFramebufferSize window
  let (L.V2 cx cy) = (fromJust $ evalState (getPlayer playerKey) world) ^. pVehicle . vPosition
      (x', y') = (2 * x / fromIntegral width - 1, (-2) * y / fromIntegral height + 1)
      angle = atan2 (realToFrac y') (realToFrac x')
  return $ clientState {world = (execState (updatePlayer playerKey (zoom pVehicle (setOrientation angle))) world)}

serverUpdateWorld :: ClientState -> IO World
serverUpdateWorld clientState@ClientState{..} = join $ atomically $ do
  emptyChan <- isEmptyTChan worldUpdateChan 
  if not emptyChan
    then do
        message <- readTChan worldUpdateChan
        return $ do
          -- Client player should keep it's position (simulated on the client)
          let playerM = evalState (getPlayer playerKey) world
              updateFunc = case playerM of
                Nothing     -> return ()
                Just player -> (pVehicle . vPosition) .= (player ^. pVehicle . vPosition) >>
                               (pVehicle . vOrientation) .= (player ^. pVehicle . vOrientation)

              world' = execState (processWorldMessage message >> updatePlayer playerKey updateFunc) world
          serverUpdateWorld clientState { world = world' }
    else do
      return $ pure world

processWorldMessage :: C.WorldMessage -> WorldS ()
processWorldMessage message =
  case message of
    C.WorldUpdate world -> put world

sendPositionUpdate :: ClientState -> IO ()
sendPositionUpdate ClientState{..} = do
  let playerM = evalState (getPlayer playerKey) world
  case playerM of
    Just player -> do
      _ <- sendMessage serverHandle (CS.WorldMessage $ CS.PositionUpdate (player ^. pVehicle . vPosition, player ^. pVehicle . vOrientation))
      return ()
    Nothing     -> return ()