{-# LANGUAGE RecordWildCards #-}

module Main where

import Network.Socket (withSocketsDo)
import Control.Concurrent (forkIO)
import Text.Printf (printf)

import GNetwork.Client (connectTo, receiver, inMessageProcessor, initialSetup)
import GState.Client (newClientState)
import GLogger.Client (initLogger, cleanLog, logInfo)

main :: IO ()
main = withSocketsDo $ do
  cleanLog
  initLogger
  serverHandle   <- connectTo "127.0.0.1" "10541"
  clientState    <- newClientState serverHandle

  logInfo (printf "Setup is done. Sending connection request")
  initialSetup clientState

  forkIO (receiver clientState)

  logInfo (printf "Starting to listen")
  inMessageProcessor clientState