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
import qualified Data.Map as Map
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

drawMenuBackground :: ClientState -> ShaderResources -> L.M33 GL.GLfloat -> Float -> IO ()
drawMenuBackground ClientState{..} shaderResources@ShaderResources{..} cameraMatrix ar = do
  case HMap.lookup "menuBackground" meshMap of
    Nothing   -> return ()
    Just mesh -> drawMesh shaderResources ((scale ar 1) L.!*! cameraMatrix) mesh


drawHUD :: ClientState -> ShaderResources -> GLFW.Window -> IO ()
drawHUD clientState@ClientState{..} shaderResources@ShaderResources{..} window = do
  (width, height) <- GLFW.getFramebufferSize window
  let ar = (fromIntegral width) / (fromIntegral height)
      transBack = translate (L.V2 (0.85 * ar) 0.90)

  let playerM = evalState (getPlayer playerKey) world
      transFront = case playerM of
        Nothing  -> transBack
        Just ply -> do
          let percentHp = (ply ^. pVehicle . vHealth) / 100
          translate (L.V2 ((0.85 - ((1 - percentHp) * 0.22) / 2) * ar) (0.90)) L.!*! scale percentHp 1
      team = case playerM of
        Nothing -> 1
        Just ply -> ply ^. pTeam

  case HMap.lookup (if team == 1 then "healthBackBlue" else "healthBackRed") meshMap of
    Nothing   -> return ()
    Just mesh -> drawMesh shaderResources transBack mesh

  case HMap.lookup (if team == 1 then "healthFrontBlue" else "healthFrontRed") meshMap of
    Nothing   -> return ()
    Just mesh -> drawMesh shaderResources transFront mesh

  drawMinimap clientState shaderResources window

drawMinimap :: ClientState -> ShaderResources -> GLFW.Window -> IO ()
drawMinimap ClientState{..} shaderResources@ShaderResources{..} window = do
  (width, height) <- GLFW.getFramebufferSize window
  let ar = (fromIntegral width) / (fromIntegral height)
      minimapTrans = translate (L.V2 (0.825 * ar) (-0.75))
      ar2 = 0.2 / 5 -- careful with this (minimap / world)

  case HMap.lookup "minimap" meshMap of
    Nothing   -> return ()
    Just mesh -> drawMesh shaderResources minimapTrans mesh

  mapM_ (\ply -> let (L.V2 x y) = (ply ^. pVehicle . vPosition) in
    if (ply ^. pClientKey == playerKey)
      then case HMap.lookup "minimapDotGreen" meshMap of
        Nothing   -> return ()
        Just mesh -> drawMesh shaderResources (translate (L.V2 (x * ar2) (y * ar2)) L.!*! minimapTrans) mesh
      else case (ply ^. pTeam) of
        1 -> case HMap.lookup "minimapDotBlue" meshMap of
              Nothing   -> return ()
              Just mesh -> drawMesh shaderResources (translate (L.V2 (x * ar2) (y * ar2)) L.!*! minimapTrans) mesh
        2 -> case HMap.lookup "minimapDotRed" meshMap of
              Nothing   -> return ()
              Just mesh -> drawMesh shaderResources (translate (L.V2 (x * ar2) (y * ar2)) L.!*! minimapTrans) mesh
    ) $ Map.filter (\ply -> ply ^. pVehicle . vHealth /= 0) $ (world ^. players)


drawWorld :: ClientState -> ShaderResources -> GLFW.Window -> IO ()
drawWorld clientState@ClientState{..} shaderResources@ShaderResources{..} window = do
  GL.clearColor GL.$= GL.Color4 1 1 1 1
  GL.clear [GL.ColorBuffer]
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

  case HMap.lookup "background" meshMap of
    Nothing   -> return ()
    Just mesh -> drawMesh shaderResources cameraTrans mesh


 
  mapM_ (\bullet -> case HMap.lookup ("bullet" ++ (show $ bullet ^. bMeshId) ++ (if (bullet ^. bTeam == 1) then "Blue" else "Red")) meshMap of
    Nothing   -> return ()
    Just mesh -> let trans = (translate $ bullet ^. bPosition) in
      drawMesh shaderResources (cameraTrans L.!*! trans) mesh) $ world ^. bullets

  mapM_ (\ply -> case HMap.lookup ("vehicle" ++ (show $ ply ^. pVehicle . vMeshId) ++ (if (ply ^. pTeam == 1) then "Blue" else "Red")) meshMap of
    Nothing   -> return ()
    Just mesh -> let trans = (translate $ ply ^. pVehicle . vPosition) L.!*! (rotate $ ply ^. pVehicle . vOrientation) in
      drawMesh shaderResources (cameraTrans L.!*! trans) mesh) $ Map.filter (\ply -> ply ^. pVehicle . vHealth /= 0) $ (world ^. players)


  drawHUD clientState shaderResources window

  menuIsOn_ <- atomically $ readTVar menuIsOn
  if menuIsOn_ == True
    then drawMenuBackground clientState shaderResources L.identity ar
    else return ()

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