{-# LANGUAGE RecordWildCards #-}

module GNetwork.Client (
  connectTo,
  sendMessage,
  receiver,
  initialSetup
  ) where

import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Data.ByteString as BS
import Control.Monad (forever, join)
import Control.Concurrent.STM (atomically, writeTChan, readTChan)
import Data.Serialize (decode, encode)
import Text.Printf (printf)

import Common.GTypes (HostName, Port, Message(..), ClientSettings(..))
import GState.Client (ClientState(..), ConnHandle(..))
import GLogger.Client (logError, logInfo)


connectTo :: HostName -> Port -> IO ConnHandle
connectTo hostName port = do
  addrInfos <- NS.getAddrInfo Nothing (Just hostName) (Just port)
  let serverAddr = head addrInfos
  sock <- NS.socket (NS.addrFamily serverAddr) NS.Datagram NS.defaultProtocol
  return $ ConnHandle sock (NS.addrAddress serverAddr)

sendMessage :: ClientState -> BS.ByteString -> IO Int
sendMessage ClientState{..} message = do
  logInfo (printf "Sent message %s" (show message))
  let (sock, addr) = (connSocket serverHandle, connAddr serverHandle)
  NSB.sendTo sock message addr 

receiver :: ClientState -> IO ()
receiver ClientState{..} = forever $ do
  recv <- NSB.recv (connSocket serverHandle) maxBytes
  let eitherMessage = decode recv
  case eitherMessage of
    Right message -> do
      atomically $ writeTChan worldMessages message
    Left _        -> logError (printf "Received non decodable message '%s'" (show recv))
 where
  maxBytes = 1024

initialSetup :: ClientState -> IO ()
initialSetup clientState = do
  let settings = ClientSettings {name = "Levi", color = "Green"}
  sendMessage clientState (encode $ ConnectionRequest settings)
  return ()