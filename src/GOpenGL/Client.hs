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

import Common.GObjects (Circle(..), Player(..), World(..))
import GState.Client (ClientState(..))
import GLogger.Client (logError, logInfo)
import GInput.Client (keyCallback)


drawCircle :: Circle -> IO ()
drawCircle (Circle (x, y) r) =
  GL.renderPrimitive GL.TriangleFan $ do
    GL.color colorRed
    GL.vertex(GL.Vertex3 x y 0)
    forM_ [0 .. triangleAmount] $ \i -> do
      GL.color colorRed
      GL.vertex(GL.Vertex3 (x + (r * cos(i * twicePi / triangleAmount))) (y + (r * sin(i * twicePi / triangleAmount))) 0)
  where
    triangleAmount = 20
    twicePi = pi * 2
    colorRed :: GL.Color3 GL.GLdouble
    colorRed = GL.Color3 1 0 0

drawWorld :: ClientState -> GLFW.Window -> IO ()
drawWorld clientState window = do
  (width, height) <- GLFW.getFramebufferSize window
  let ratio = fromIntegral width / fromIntegral height
  GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
  GL.clear [GL.ColorBuffer]
  GL.matrixMode GL.$= GL.Projection
  GL.loadIdentity
  GL.ortho (negate ratio) ratio (negate 1.0) 1.0 1.0 (negate 1.0)
  -- logInfo (printf "draw : %s" (show $ world clientState))
  mapM_ (\player -> drawCircle (circle player)) (players $ world clientState)
  GLFW.swapBuffers $ window
  GLFW.pollEvents

withOpenGL :: ClientState -> (GLFW.Window -> IO ()) -> IO ()
withOpenGL clientState func = do
  success <- GLFW.init
  if success
    then do
      window <- GLFW.createWindow 640 480 "Haskell Game" Nothing Nothing
      GLFW.makeContextCurrent window
      flip (maybe (GLFW.terminate >> exitFailure)) window $ \window' -> do
        GLFW.setKeyCallback window' (Just $ keyCallback clientState)
        func window'
        GLFW.destroyWindow window'
      GLFW.terminate
    else do
      logError (printf "Could not initialize GLFW")
      exitFailure