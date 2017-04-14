{-# LANGUAGE RecordWildCards #-}

module Main where

import Network.Socket (withSocketsDo)
import Control.Concurrent.STM (atomically)
import Text.Printf (printf)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)

import Common.GTypes (Message(..))
import GState.Server (Server(..), newServer)
import GNetwork.Server (listenTo, masterReceiver, broadcast)
import GLogger.Server (initLogger, cleanLog, logInfo)


updates :: Server -> IO ()
updates server = forever $ do
  atomically $ broadcast server WorldUpdateTest
  threadDelay 16000 -- 0.016 sec

main :: IO ()
main = withSocketsDo $ do
  cleanLog
  initLogger

  messageSock <- listenTo port
  server <- newServer messageSock
  logInfo (printf "Listening on port %s" port)

  forkIO (updates server)
  masterReceiver server
 where
  port = "10541"
