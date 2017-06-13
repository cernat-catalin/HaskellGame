{-# LANGUAGE RecordWildCards #-}

module GNetwork.Client (
  connectTo,
  sendMessage,
  receiver
  ) where

import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import Control.Monad (forever)
import Control.Concurrent.STM (atomically, writeTChan)
import Data.Serialize (decode, encode)
import Text.Printf (printf)

import GCommon.Types.Generic (ConnHandle(..), HostName, Port)
import GState.Client (ClientState(..))
import GLogger.Client (logError)
import GMessages.Network.Converter (convert)
import qualified GMessages.Network.ServerClient as SC
import qualified GMessages.Network.ClientServer as CS
import qualified GMessages.Client as C



connectTo :: HostName -> Port -> IO ConnHandle
connectTo hostName port = do
  addrInfos <- NS.getAddrInfo Nothing (Just hostName) (Just port)
  let serverAddr = head addrInfos
  sock <- NS.socket (NS.addrFamily serverAddr) NS.Datagram NS.defaultProtocol
  return $ ConnHandle sock (NS.addrAddress serverAddr)

sendMessage :: ConnHandle -> CS.Message -> IO Int
sendMessage ConnHandle{..} message = do
  let (sock, addr) = (connSocket, connAddr)
  NSB.sendTo sock (encode message) addr

receiver :: ClientState -> IO ()
receiver clientState@ClientState{..} = forever $ do
  recv <- NSB.recv (connSocket serverHandle) maxBytes
  let eitherMessage = decode recv
  case eitherMessage of
    Right message -> messageAssigner clientState message
    Left _        -> logError (printf "Received non decodable message '%s'" (show recv))
 where
  maxBytes = 1024

messageAssigner :: ClientState -> SC.Message -> IO ()
messageAssigner ClientState{..} message = atomically $ do
  case message of
    SC.WorldMessage worldMessage     -> writeTChan worldUpdateChan (convert worldMessage)
    SC.ServiceMessage serviceMessage -> case (convert serviceMessage) of
      C.PingMessage pingMessage -> writeTChan pingSvcChan pingMessage