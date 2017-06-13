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


import GMessages.Network.ClientServer (Message(..), ConnectionMessage(..), ServiceMessage(..))
import GNetwork.Client (sendMessage)
import GCommon.Objects.Objects as GO
import GState.Client (ClientState(..))
import GLogger.Client (logError)
import GInput.Client (keyCallback)
import GOpenGL.Meshes (ShaderResources(..), drawMesh)
import GFunc.Client.Setup (shaderResourcesSetup)
import GCommon.Geometry (translate, rotate)
import GCommon.Objects.Transforms (getPlayer)


drawWorld :: ClientState -> ShaderResources -> GLFW.Window -> IO ()
drawWorld clientState@ClientState{..} shaderResources@ShaderResources{..} window = do
  GL.clearColor GL.$= GL.Color4 1 1 1 1
  GL.clear [GL.ColorBuffer]
  -- In C++ example GLUT handles resizing the viewport?
  (width, height) <- GLFW.getFramebufferSize window
  GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
  GL.currentProgram GL.$= (Just . U.program $ program)



  let playerM = evalState (getPlayer playerKey) world
      cameraTrans = case playerM of
        Nothing  -> L.identity
        Just ply -> translate $ -(ply ^. vehicle . position)

  case HMap.lookup 0 meshMap of
    Nothing   -> return ()
    Just mesh -> drawMesh shaderResources cameraTrans mesh

  case HMap.lookup (-1) meshMap of
    Nothing -> return ()
    Just mesh -> drawMesh shaderResources cameraTrans mesh

  mapM_ (\ply -> case HMap.lookup (ply ^. vehicle . meshId) meshMap of
    Nothing   -> return ()
    Just mesh -> let trans = (translate $ ply ^. vehicle . position) L.!*! (rotate $ ply ^. vehicle . orientation) in
      drawMesh shaderResources (cameraTrans L.!*! trans) mesh) $ world ^. players

  mapM_ (\bullet -> case HMap.lookup 1 meshMap of
    Nothing   -> return ()
    Just mesh -> let trans = (translate $ bullet ^. bposition) in
      drawMesh shaderResources (cameraTrans L.!*! trans) mesh) $ world ^. bullets

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