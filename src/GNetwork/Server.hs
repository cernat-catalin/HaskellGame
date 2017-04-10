{-# LANGUAGE RecordWildCards #-}

module GNetwork.Server (
  listenTo,
  receiver,
  inMessageProcessor,
  outMessageProcessor,
  sendMessage,
  broadcast
  ) where

import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (STM, atomically, readTChan, readTVar, writeTChan)
import Control.Monad (forever, join, when)
import Data.Serialize (decode, encode)

import GState.Server (Server(..), Client(..), addClient)
import Common.GTypes (Port, Message(..), ClientMessage(..))


listenTo :: Port -> IO NS.Socket
listenTo port = do
  addrInfos <- NS.getAddrInfo
              (Just (NS.defaultHints { NS.addrFlags = [NS.AI_PASSIVE] }))
              (Just "127.0.0.1")
              (Just port)
  let serverAddr = head addrInfos
  sock <- NS.socket (NS.addrFamily serverAddr) NS.Datagram NS.defaultProtocol
  NS.bind sock (NS.addrAddress serverAddr)
  return sock

receiver :: Server -> IO ()
receiver Server{..} = forever $ do
  (recv, addr) <- NSB.recvFrom messageSocket maxBytes
  putStrLn $ "Received from: " ++ (show addr) ++ " - " ++ (BS8.unpack recv)
  let eitherMessage = decode recv
  putStrLn $ "Message was: " ++ (show eitherMessage)
  case eitherMessage of
    Right message -> atomically $ writeTChan inMessageChan (ClientMessage addr message)
    Left err      -> putStrLn $ "Received non message: " ++ err
 where
  maxBytes = 1024

inMessageProcessor :: Server -> IO ()
inMessageProcessor server@Server{..} = join $ atomically $ do
  (ClientMessage addr message) <- readTChan inMessageChan  
  return $ do
    case message of
      ConnectionRequest -> do
        ok <- addClient server addr
        case ok of
          Just client -> do
            Prelude.putStrLn $ "Client: " ++ (show client) ++ " connected"
            forkIO (outMessageProcessor server client)
            return ()
          Nothing     -> Prelude.putStrLn $ "A client with an addr already in use tried to connects"
      _                 -> Prelude.putStrLn $ "unknown message type: " ++ (show message)
    inMessageProcessor server

-- TODO: what if client force quits ? who kills the thread then?
outMessageProcessor :: Server -> Client -> IO ()
outMessageProcessor server@Server{..} client@Client{..} = join $ atomically $ do
  message <- readTChan outMessageChan
  return $ do
    NSB.sendTo messageSocket (encode message) clientAddr
    when (message /= ConnectionTerminated) $ outMessageProcessor server client

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg =
  writeTChan outMessageChan msg

-- TODO research unbounded STM
broadcast :: Server -> Message -> STM ()
broadcast Server{..} msg = do
  clientMap <- readTVar clients
  mapM_ (\client -> sendMessage client msg) (Map.elems clientMap)