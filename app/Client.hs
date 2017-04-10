{-# LANGUAGE RecordWildCards #-}

module Main where

import Network.Socket (withSocketsDo)
import Control.Concurrent (forkIO)
import Text.Printf (printf)
import Data.Serialize (encode)

import GNetwork.Client (connectTo, receiver, sendMessage, inMessageProcessor)
import GState.Client (newClientState)
import Common.GTypes (Message(..))

main :: IO ()
main = withSocketsDo $ do
  serverHandle   <- connectTo "127.0.0.1" "10541"
  clientState    <- newClientState serverHandle

  printf "Setup is done. Sending connection request.\n"
  sendMessage clientState (encode ConnectionRequest)

  forkIO (receiver clientState)

  printf "Starting to listen\n"
  inMessageProcessor clientState