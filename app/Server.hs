{-# LANGUAGE RecordWildCards #-}

module Main where

import Network.Socket (withSocketsDo)
import Control.Concurrent.STM (atomically)
import Text.Printf (printf)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)

import Common.GTypes (Message(..))
import GState.Server (Server(..), newServer)
import GNetwork.Server (listenTo, receiver, inMessageProcessor, broadcast)


updates :: Server -> IO ()
updates server = forever $ do
  atomically $ broadcast server WorldUpdateTest
  threadDelay 16000 -- 0.016 sec

main :: IO ()
main = withSocketsDo $ do
  inMessageSock <- listenTo port
  server <- newServer inMessageSock

  printf "Listening on port %s\n" port

  forkIO (updates server)
  forkIO (inMessageProcessor server)
  receiver server
 where
  port = "10541"
