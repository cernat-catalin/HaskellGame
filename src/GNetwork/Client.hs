{-# LANGUAGE RecordWildCards #-}

module GNetwork.Client (
  connectTo,
  sendMessage,
  receiver,
  inMessageProcessor
  ) where

import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Data.ByteString as BS
import Control.Monad (forever, join)
import Control.Concurrent.STM (atomically, writeTChan, readTChan)
import Data.Serialize (decode)

import Common.GTypes (HostName, Port)
import GState.Client (ClientState(..), ConnHandle(..))


connectTo :: HostName -> Port -> IO ConnHandle
connectTo hostName port = do
  addrInfos <- NS.getAddrInfo Nothing (Just hostName) (Just port)
  let serverAddr = head addrInfos
  sock <- NS.socket (NS.addrFamily serverAddr) NS.Datagram NS.defaultProtocol
  return $ ConnHandle sock (NS.addrAddress serverAddr)

sendMessage :: ClientState -> BS.ByteString -> IO Int
sendMessage ClientState{..} message = do
  let (sock, addr) = (connSocket serverHandle, connAddr serverHandle)
  NSB.sendTo sock message addr 

receiver :: ClientState -> IO ()
receiver ClientState{..} = forever $ do
  recv <- NSB.recv (connSocket serverHandle) maxBytes
  let eitherMessage = decode recv
  case eitherMessage of
    Right message -> atomically $ writeTChan serverInChan message
    Left  err     -> putStrLn $ "Received non message: " ++ err
 where
  maxBytes = 1024

-- TODO: thinks about the life of this thread
inMessageProcessor :: ClientState -> IO ()
inMessageProcessor clientState@ClientState{..} = join $ atomically $ do
  message <- readTChan serverInChan
  return $ do
    putStrLn $ show message
    inMessageProcessor clientState