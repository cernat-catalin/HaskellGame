{-# LANGUAGE RecordWildCards #-}

module GNetwork.Client (
  connectTo,
  sendMessage,
  receiver
  ) where

import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Data.ByteString as BS
import Control.Monad (forever)
import Control.Concurrent.STM (atomically, writeTChan)
import Data.Serialize (decode, encode)
import Text.Printf (printf)

import Common.GTypes (HostName, Port, ClientSettings(..))
import GMessages.Network.ServerClient (Message(..), ServiceMessage(..), WorldMessage(..))
import qualified GMessages.Client as C
import GMessages.Network.Converter (convert)
import GState.Client (ClientState(..), ConnHandle(..))
import GLogger.Client (logError)


connectTo :: HostName -> Port -> IO ConnHandle
connectTo hostName port = do
  addrInfos <- NS.getAddrInfo Nothing (Just hostName) (Just port)
  let serverAddr = head addrInfos
  sock <- NS.socket (NS.addrFamily serverAddr) NS.Datagram NS.defaultProtocol
  return $ ConnHandle sock (NS.addrAddress serverAddr)

-- TODO refactor to send message not ByteString
sendMessage :: ClientState -> BS.ByteString -> IO Int
sendMessage ClientState{..} message = do
  let (sock, addr) = (connSocket serverHandle, connAddr serverHandle)
  NSB.sendTo sock message addr 

receiver :: ClientState -> IO ()
receiver clientState@ClientState{..} = forever $ do
  recv <- NSB.recv (connSocket serverHandle) maxBytes
  let eitherMessage = decode recv
  case eitherMessage of
    Right message ->
      case message of
        WorldMessage worldMessage    ->
          case worldMessage of
            WorldUpdate _ -> atomically $ writeTChan worldUpdateChan (convert worldMessage)
        ServiceMessage serviceMessage -> messageSplitter clientState (convert serviceMessage)
    Left _        -> logError (printf "Received non decodable message '%s'" (show recv))
 where
  maxBytes = 1024

messageSplitter :: ClientState -> C.ServiceMessage -> IO ()
messageSplitter ClientState{..} message = atomically $ do
  case message of
    C.PingMessage pingMessage -> writeTChan pingSvcChan pingMessage