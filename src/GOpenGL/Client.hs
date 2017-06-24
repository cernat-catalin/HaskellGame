{-# LANGUAGE RecordWildCards #-}

module GOpenGL.Client (
  withOpenGL,
  drawWorld
  ) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Control.Monad (forM_)
import System.Exit (exitFailure)
import Text.Printf (printf)
import Control.Concurrent (myThreadId, ThreadId)
import Control.Exception (throwTo)
import System.Exit (ExitCode(..))
import System.Posix.Signals (installHandler, keyboardSignal, Handler(..))
import Control.Lens ((^.))
import qualified Data.HashMap.Strict as HMap
import qualified Graphics.GLUtil as U
import qualified Linear as L
import Control.Monad.State (put, evalState, execState, modify)
import Control.Concurrent.STM (TVar, readTVar, atomically)
import Graphics.GLUtil (asUniform, getUniform, ShaderProgram)


import GMessages.Network.ClientServer (Message(..), ConnectionMessage(..), ServiceMessage(..))
import GNetwork.Client (sendMessage)
import GCommon.Objects.Objects as GO
import GState.Client (ClientState(..))
import GLogger.Client (logError)
import GInput.Client (keyCallback)
import GOpenGL.Meshes (ShaderResources(..), drawMesh)
import GFunc.Client.Setup (shaderResourcesSetup)
import GCommon.Geometry (translate, rotate, scale)
import GCommon.Objects.Transforms (getPlayer)

drawMenuBlock :: ClientState -> ShaderResources -> L.M33 GL.GLfloat -> IO ()
drawMenuBlock ClientState{..} shaderResources@ShaderResources{..} cameraMatrix = do
  case HMap.lookup (-2) meshMap of
    Nothing   -> return ()
    Just mesh -> drawMesh shaderResources cameraMatrix mesh


drawHUD :: ClientState -> ShaderResources -> IO ()
drawHUD ClientState{..} shaderResources@ShaderResources{..} = do
  let trans = translate (L.V2 0.7 0.8)
  case HMap.lookup (-3) meshMap of
    Nothing   -> return ()
    Just mesh -> drawMesh shaderResources trans mesh


drawWorld :: ClientState -> ShaderResources -> GLFW.Window -> IO ()
drawWorld clientState@ClientState{..} shaderResources@ShaderResources{..} window = do
  GL.clearColor GL.$= GL.Color4 1 1 1 1
  GL.clear [GL.ColorBuffer]
  -- In C++ example GLUT handles resizing the viewport?
  (width, height) <- GLFW.getFramebufferSize window
  GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
  GL.currentProgram GL.$= (Just . U.program $ program)

  let ar = (fromIntegral width) / (fromIntegral height);
  let projectionMatrix = L.ortho (-ar) ar (-1) 1 (-1) 1 :: L.M44 GL.GLfloat

  asUniform projectionMatrix $ getUniform program projectionUnif


  let playerM = evalState (getPlayer playerKey) world
      cameraTrans = case playerM of
        Nothing  -> L.identity
        Just ply -> (scale 0.5 0.5) L.!*! (translate $ -(ply ^. pVehicle . vPosition))

  case HMap.lookup 0 meshMap of
    Nothing   -> return ()
    Just mesh -> drawMesh shaderResources cameraTrans mesh

  case HMap.lookup (-1) meshMap of
    Nothing -> return ()
    Just mesh -> drawMesh shaderResources cameraTrans mesh


  menuIsOn_ <- atomically $ readTVar menuIsOn
  if menuIsOn_ == True
    then drawMenuBlock clientState shaderResources L.identity
    else return ()

 

  mapM_ (\ply -> case HMap.lookup (ply ^. pVehicle . vMeshId) meshMap of
    Nothing   -> return ()
    Just mesh -> let trans = (translate $ ply ^. pVehicle . vPosition) L.!*! (rotate $ ply ^. pVehicle . vOrientation) in
      drawMesh shaderResources (cameraTrans L.!*! trans) mesh) $ world ^. players

  mapM_ (\bullet -> case HMap.lookup 1 meshMap of
    Nothing   -> return ()
    Just mesh -> let trans = (translate $ bullet ^. bPosition) in
      drawMesh shaderResources (cameraTrans L.!*! trans) mesh) $ world ^. bullets

  drawHUD clientState shaderResources

  GLFW.swapBuffers $ window
  GLFW.pollEvents


withOpenGL :: ClientState -> (ShaderResources -> GLFW.Window -> IO ()) -> IO ()
withOpenGL clientState func = do
  success <- GLFW.init
  if success
    then do
      GLFW.windowHint $ GLFW.WindowHint'Samples 4
      window <- GLFW.createWindow 640 480 "Haskell Game" Nothing Nothing
      GLFW.makeContextCurrent window
      flip (maybe (GLFW.terminate >> exitFailure)) window $ \window' -> do
        GLFW.setKeyCallback window' (Just $ keyCallback clientState)
        tid <- myThreadId
        _ <- installHandler keyboardSignal (Catch $ exitGame clientState tid) Nothing
        shaderResources <- shaderResourcesSetup
        func shaderResources window'
        GLFW.destroyWindow window'
      GLFW.terminate
    else do
      logError (printf "Could not initialize GLFW")
      exitFailure

exitGame :: ClientState -> ThreadId -> IO ()
exitGame ClientState{..} id' = do
  _ <- sendMessage serverHandle (ServiceMessage $ ConnectionMessage $ ConnectionTerminated)
  throwTo id' ExitSuccess