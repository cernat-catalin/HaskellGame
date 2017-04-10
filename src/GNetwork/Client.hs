{-# LANGUAGE RecordWildCards #-}

module GNetwork.Client (
  connectTo,
  sendMessage,
  receiver
  ) where

import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Data.ByteString as BS
import Network (Socket)
import Control.Monad (forever)
import Control.Concurrent.STM
import Data.Serialize (decode)

import Common.GTypes (HostName, Port, Message)
import GState.Client (ClientState(..), ConnHandle(..))


connectTo :: HostName -> Port -> IO ConnHandle
connectTo hostName port = do
  addrInfos <- NS.getAddrInfo Nothing (Just hostName) (Just port)
  let serverAddr = head addrInfos
  sock <- NS.socket (NS.addrFamily serverAddr) NS.Datagram NS.defaultProtocol
  return $ ConnHandle sock (NS.addrAddress serverAddr)

initialSetup :: HostName -> Port -> IO Socket
initialSetup hostName port = undefined

sendMessage :: ClientState -> BS.ByteString -> IO Int
sendMessage ClientState{..} message = do
  let (sock, addr) = (connSocket serverHandle, connAddr serverHandle)
  NSB.sendTo sock message addr 

receiver :: ClientState -> IO ()
receiver ClientState{..} = forever $ do
  recv <- NSB.recv (connSocket serverHandle) maxBytes
  let eitherMessage = (decode recv) :: Either String Message
  case eitherMessage of
    Right message -> atomically $ writeTChan serverInChan message
    Left  err     -> putStrLn $ "Received non message: " ++ err
 where
  maxBytes = 1024