{-# LANGUAGE RecordWildCards #-}

module Main where

import Network.Socket (withSocketsDo)
import Control.Concurrent (forkIO)
import Text.Printf (printf)
import Data.Serialize (encode)

import GNetwork.Client (connectTo, receiver, sendMessage, inMessageProcessor)
import GState.Client (newClientState)
import Common.GTypes (Message(..))
import GLogger.Client (initLogger, cleanLog, logInfo)

main :: IO ()
main = withSocketsDo $ do
  cleanLog
  initLogger
  serverHandle   <- connectTo "127.0.0.1" "10541"
  clientState    <- newClientState serverHandle

  logInfo (printf "Setup is done. Sending connection request")
  sendMessage clientState (encode ConnectionRequest)

  forkIO (receiver clientState)

  logInfo (printf "Starting to listen")
  inMessageProcessor clientState