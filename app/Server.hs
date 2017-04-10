{-# LANGUAGE RecordWildCards #-}

module Main where

import Network.Socket as NS
import Network
import Network.Socket.ByteString as NBS
import Data.ByteString.Char8 as BS8
import Control.Concurrent.STM
import Text.Printf (printf)
import Control.Exception.Base (finally)
import Control.Concurrent (forkFinally, forkIO, threadDelay)
import Control.Monad (forever, join)
import Control.Concurrent.Async
import qualified Data.Map as Map

import Common.GTypes
import GState.Server
import GNetwork.Server


updates :: Server -> IO ()
updates server = forever $ do
  atomically $ broadcast server WorldUpdateTest
  threadDelay 16000 -- 0.016 sec

processOutMessages :: Server -> IO ()
processOutMessages server@Server{..} = join $ atomically $ do
  clientMap <- readTVar clients
  return $ do
    mapM_ (\client -> sendMessagesIO server client) (Map.elems clientMap)
    threadDelay 16000
    processOutMessages server
 where

processInMessages :: Server -> IO ()
processInMessages server@Server{..} = join $ atomically $ do
  (ClientMessage addr message) <- readTChan inMessageChan  
  return $ do
    case message of
      ConnectionRequest -> do
        ok <- addClient server addr
        case ok of
          Just client -> Prelude.putStrLn $ "Client: " ++ (show client) ++ " connected"
          Nothing     -> Prelude.putStrLn $ "A client with an addr already in use tried to connects"
      _                 -> Prelude.putStrLn $ "unknown message type: " ++ (show message)
    processInMessages server
      

main :: IO ()
main = withSocketsDo $ do
  inMessageSock <- listenTo port
  server <- newServer inMessageSock
  printf "Listening on port %s\n" port

  forkIO (updates server)
  forkIO (processOutMessages server)
  forkIO (processInMessages server)

  receiver server
 where
  port = "10541"
