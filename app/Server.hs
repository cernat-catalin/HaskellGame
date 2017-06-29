{-# LANGUAGE RecordWildCards #-}

module Main where

import Network.Socket (withSocketsDo)
import Text.Printf (printf)
import Control.Concurrent (forkIO)

import GState.Server (newServer)
import GNetwork.Server (listenTo, masterReceiver)
import GLogger.Server (initLogger, cleanLog, logInfo)
import GMainLoop.Server (mainLoop)
import GServices.Server (connectionService, pingService)


main :: IO ()
main = withSocketsDo $ do
  cleanLog
  initLogger

  putStrLn "Enter ip:"
  ip <- getLine
  putStrLn "Enter port:"
  port <- getLine
  -- let ip   = "127.0.0.1"
  --     port = "10541"

  messageSock <- listenTo ip port
  server <- newServer messageSock
  logInfo (printf "Listening on port %s" port)

  _ <- forkIO (masterReceiver server)
  _ <- forkIO (connectionService server)
  _ <- forkIO (pingService server)
  mainLoop server