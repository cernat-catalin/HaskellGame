{-# LANGUAGE RecordWildCards #-}

module Main where

import Network.Socket (withSocketsDo)
import Control.Concurrent (forkIO, threadDelay, myThreadId, ThreadId)
import Text.Printf (printf)
import System.Exit (exitFailure, exitSuccess, ExitCode(..))
import Data.Maybe (maybe)
import Control.Monad (forM_)

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL

import GNetwork.Client (connectTo, receiver, initialSetup, sendMessage)
import GState.Client (ClientState(..), newClientState)
import GLogger.Client (initLogger, cleanLog, logInfo, logError)
import Common.GObjects (Circle(..), Player(..), World(..))
import GMainLoop.Client (mainLoop)
import GOpenGL.Client (withOpenGL)
import GServices.Client (processServicesMessages)
import System.Posix.Signals (installHandler, keyboardSignal, Handler(..))
import qualified Control.Exception as E
import Data.Serialize (decode, encode)
import Common.GTypes (Message(..))

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
  forkIO (receiver clientState)
  forkIO (processServicesMessages clientState)
  tid <- myThreadId
  installHandler keyboardSignal (Catch $ endLife clientState tid) Nothing
  withOpenGL clientState (mainLoop clientState)

endLife :: ClientState -> ThreadId -> IO ()
endLife clientState@ClientState{..} id = do
  sendMessage clientState (encode Quit)
  E.throwTo id ExitSuccess
