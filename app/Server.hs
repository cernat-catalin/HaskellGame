{-# LANGUAGE RecordWildCards #-}

module Main where

import Network.Socket as NS
import Network
import Network.Socket.ByteString as NBS
import Data.ByteString.Char8 as BS8
import Control.Concurrent.STM
import Text.Printf (printf)
import Control.Exception.Base (finally)
import Control.Concurrent (forkFinally)
import Control.Monad (forever, when, join)
import Control.Concurrent.Async

import Common.GTypes
import GState.Server
import GNetwork.Server

runClient :: Server -> Client -> IO ()
runClient serv@Server{..} client@Client{..} = do
  race receiver server
  return ()
 where
  receiver = forever $ do
    msg <- NBS.recv clientSocket 1024
    Prelude.putStrLn $ "received: " ++ (show msg) ++ " from: " ++ (show clientSocket)
    atomically $ sendMessage client RandomObject
  
  server = join $ atomically $ do
    msg <- readTChan clientSendChan
    return $ do
      continue <- handleMessage serv client msg
      when continue $ server

handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage _ Client{..} msg =
  case msg of
    RandomObject -> do
      NBS.send clientSocket $ BS8.pack "World update"
      return True

talk :: Socket -> Server -> IO ()
talk sock server@Server{..} = readName
 where
  readName = do
    name <- NBS.recv sock 1024
    if BS8.null name
      then readName
      else do
        ok <- addClient server (BS8.unpack name) sock
        case ok of
          Nothing -> do
            NBS.send sock $ BS8.pack "Invalid name"
            readName
          Just client -> runClient server client `finally` removeClient server (BS8.unpack name)

main :: IO ()
main = withSocketsDo $ do
  server <- newServer
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  forever $ do
    (sock', _) <- NS.accept sock
    printf "Accepted connection from %s\n" (show sock')
    forkFinally (talk sock' server) (\e -> do print e; NS.close sock')
    
port :: Int
port = 44444
