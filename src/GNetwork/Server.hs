{-# LANGUAGE RecordWildCards #-}

module GNetwork.Server (
  listenTo,
  masterReceiver,
  sendMessage,
  broadcast,
  clientSender,
  sendMessageRaw
  ) where

import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Control.Concurrent.STM (STM, atomically, readTChan, readTVar, readTVarIO, writeTChan)
import Control.Monad (forever, join)
import Data.Serialize (decode, encode)
import Text.Printf (printf)

import GState.Server (Server(..), Client(..), lookupClient)
import GCommon.Types.Generic (Port, ClientKey)
import GLogger.Server (logError)
import GMessages.Network.Converter (convert, convertWithKey, connectionRequestConverter)
import GMessages.Network.ClientServer as CS
import GMessages.Network.ServerClient as SC
import GMessages.Server as S



listenTo :: String -> Port -> IO NS.Socket
listenTo ip port = do
  addrInfos <- NS.getAddrInfo
              (Just (NS.defaultHints { NS.addrFlags = [NS.AI_PASSIVE] }))
              (Just ip)
              (Just port)
  let serverAddr = head addrInfos
  sock <- NS.socket (NS.addrFamily serverAddr) NS.Datagram NS.defaultProtocol
  NS.bind sock (NS.addrAddress serverAddr)
  return sock

sendMessage :: Client -> SC.Message -> STM ()
sendMessage Client{..} msg =
  writeTChan outMessageChan msg

sendMessageRaw :: Server -> Client -> BS.ByteString -> IO ()
sendMessageRaw Server{..} Client{..} message = do
  _ <- NSB.sendTo messageSocket message address
  return ()

broadcast :: Server -> SC.Message -> STM ()
broadcast Server{..} msg = do
  clientMap <- readTVar clients
  mapM_ (\client -> sendMessage client msg) (Map.elems clientMap)

masterReceiver :: Server -> IO ()
masterReceiver server@Server{..} = forever $ do
  (recv, addr) <- NSB.recvFrom messageSocket maxBytes
  let eitherMessage = decode recv
  case eitherMessage of
    Left _        -> logError (printf "From '%s' received non decodable message %s" (show addr) (show recv))
    Right message -> messageAssigner server addr message
 where
  maxBytes = 65507

messageAssigner :: Server -> NS.SockAddr -> CS.Message -> IO ()
messageAssigner Server{..} addr message = atomically $ do
  case message of
    CS.WorldMessage key worldMessage           -> writeTChan worldChan (convertWithKey worldMessage key)
    CS.ServiceMessage key serviceMessage -> case serviceMessage of
      CS.ConnectionMessage connectionMessage -> do
        case connectionMessage of
          CS.ConnectionRequest settings -> do
            clients' <- readTVar clients
            let nextKey = Map.foldrWithKey (\k a b -> max k b) 0 clients' + 1
            writeTChan connectionSvcChan (connectionRequestConverter connectionMessage nextKey addr)
          CS.ConnectionTerminated -> writeTChan connectionSvcChan (connectionRequestConverter connectionMessage key addr)
      CS.PingMessage pingMessage             -> writeTChan pingSvcChan (convertWithKey pingMessage key)

clientSender :: Server -> Client -> IO ()
clientSender Server{..} Client{..} = forever $ join $ atomically $ do
  message <- readTChan outMessageChan
  return $ do
    NSB.sendTo messageSocket (encode message) address
