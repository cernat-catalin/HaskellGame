{-# LANGUAGE RecordWildCards #-}

module Main where

import Network.Socket (withSocketsDo)
import Control.Concurrent (forkIO)
import Text.Printf (printf)

import GNetwork.Client (connectTo, receiver)
import GLogger.Client (initLogger, cleanLog, logInfo)
import GMainLoop.Client (mainLoop)
import GOpenGL.Client (withOpenGL)
import GServices.Client (settingsService, pingService, serverOutService, menuService)
import GFunc.Client.Setup (initialSetup)
import GState.Client (ClientState(..))

main :: IO ()
main = withSocketsDo $ do
  cleanLog
  initLogger
  serverHandle   <- connectTo "127.0.0.1" "10541"
  clientState    <- initialSetup serverHandle

  logInfo (printf "Client playerKey: %s" (show $ playerKey clientState))
  logInfo (printf "Starting to listen")

  -- NOTE: Don't start GLFW and OpenGL with forkIO
  -- weird things happen. Possibly swap buffers isn't called correctly (is delayed)
  -- Found this: GLFW doesn't work well with GHC threads, forkIO or threadDelay. So avoid them if you can.
  _ <- forkIO (receiver clientState)
  _ <- forkIO (settingsService clientState)
  _ <- forkIO (pingService clientState)
  _ <- forkIO (serverOutService clientState)
  _ <- forkIO (menuService clientState)
  withOpenGL clientState (mainLoop clientState)