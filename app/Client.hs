{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (forkFinally)
import Network.Socket (withSocketsDo)
import Control.Concurrent.STM
import Text.Printf (printf)
import Data.Serialize (encode)
import Control.Monad (join)

import GNetwork.Client (connectTo, receiver, sendMessage)
import GState.Client (newClientState, ClientState(..))
import Common.GTypes (Message(..))

handleMessages :: ClientState -> IO ()
handleMessages clientState@ClientState{..} = join $ atomically $ do
    msg <- readTChan $ serverInChan
    return $ do
      putStrLn $ show msg
      handleMessages clientState

main :: IO ()
main = withSocketsDo $ do
  serverHandle   <- connectTo "127.0.0.1" "10541"
  clientState    <- newClientState serverHandle

  printf "Setup is done. Sending connection request.\n"

  printf "Socket: %s\n" (show serverHandle)

  sendMessage clientState (encode ConnectionRequest)

  forkFinally (receiver clientState) (\e -> do print e;)

  printf "Starting to listen\n"

  handleMessages clientState