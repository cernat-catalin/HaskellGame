{-# LANGUAGE RecordWildCards #-}

module GNetwork.Server (
  sendMessage,
  broadcast,
  listenTo,
  receiver,
  sendMessagesIO
  ) where

import Control.Concurrent.STM
import Control.Monad (forever, when, join)
import Data.Serialize (decode, encode)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map

import GState.Server
import Common.GTypes


-- TODO refactor this
listenTo :: Port -> IO NS.Socket
listenTo port = do
  addrInfos <-
    NS.getAddrInfo
      (Just
         (NS.defaultHints
          { NS.addrFlags = [NS.AI_PASSIVE]
          }))
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
  let eitherMessage = (decode recv) :: Either String Message
  putStrLn $ "Message was: " ++ (show eitherMessage)
  case eitherMessage of
    Right message -> do
      atomically $ writeTChan inMessageChan (ClientMessage addr message)
    Left  err     -> putStrLn $ "Received non message: " ++ err
 where
  maxBytes = 1024


--TODO refactor this 3 functions (mostly their names)

sendMessagesIO :: Server -> Client -> IO ()
sendMessagesIO server@Server{..} client@Client{..} = join $ atomically $ do
  isEmpty <- isEmptyTChan outMessageChan
  if isEmpty
    then return $ pure ()
    else do
      message <- readTChan outMessageChan  
      return $ do
        NSB.sendTo messageSocket (encode message) clientAddr
        sendMessagesIO server client

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg =
  writeTChan outMessageChan msg

broadcast :: Server -> Message -> STM ()
broadcast Server{..} msg = do
  clientMap <- readTVar clients
  mapM_ (\client -> sendMessage client msg) (Map.elems clientMap)