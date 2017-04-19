{-# LANGUAGE RecordWildCards #-}

module Main where

import Network.Socket (withSocketsDo)
import Text.Printf (printf)
import Control.Concurrent (forkIO)

import GState.Server (newServer)
import GNetwork.Server (listenTo, masterReceiver)
import GLogger.Server (initLogger, cleanLog, logInfo)
import GMainLoop.Server (mainLoop)


main :: IO ()
main = withSocketsDo $ do
  cleanLog
  initLogger

  messageSock <- listenTo port
  server <- newServer messageSock
  logInfo (printf "Listening on port %s" port)

  forkIO (masterReceiver server)
  mainLoop server
 where
  port = "10541"