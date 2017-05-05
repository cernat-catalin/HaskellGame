{-# LANGUAGE RecordWildCards #-}

module Main where

import Network.Socket (withSocketsDo)
import Control.Concurrent (forkIO)
import Text.Printf (printf)

import GNetwork.Client (connectTo, receiver)
import GState.Client (newClientState)
import GLogger.Client (initLogger, cleanLog, logInfo)
import GMainLoop.Client (mainLoop)
import GOpenGL.Client (withOpenGL)
import GServices.Client (settingsService, pingService)
import GFunc.Client.Setup (initialSetup)

main :: IO ()
main = withSocketsDo $ do
  cleanLog
  initLogger
  serverHandle   <- connectTo "127.0.0.1" "10541"
  clientState    <- newClientState serverHandle

  logInfo (printf "Setup is done. Sending connection request")
  initialSetup clientState


  logInfo (printf "Starting to listen")

  -- NOTE: Don't start GLFW and OpenGL with forkIO
  -- weird things happen. Possibly swap buffers isn't called correctly (is delayed)
  -- Found this: GLFW doesn't work well with GHC threads, forkIO or threadDelay. So avoid them if you can.
  _ <- forkIO (receiver clientState)
  _ <- forkIO (settingsService clientState)
  _ <- forkIO (pingService clientState)
  withOpenGL clientState (mainLoop clientState)