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
import Control.Concurrent.STM (STM, atomically, readTChan, readTVar, writeTChan)
import Control.Monad (forever, join)
import Data.Serialize (decode, encode)
import Text.Printf (printf)

import GState.Server (Server(..), Client(..), lookupClient)
import Common.GTypes (Port, ClientKey)
import GLogger.Server (logError)
import GMessages.Network.Converter (convert, convertWithKey)
import GMessages.Network.ClientServer as CS
import GMessages.Network.ServerClient as SC



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

sendMessage :: Client -> SC.Message -> STM ()
sendMessage Client{..} msg =
  writeTChan outMessageChan msg

sendMessageRaw :: Server -> Client -> BS.ByteString -> IO ()
sendMessageRaw Server{..} Client{..} message = do
  _ <- NSB.sendTo messageSocket message key
  return ()

-- TODO research unbounded STM
broadcast :: Server -> SC.Message -> STM ()
broadcast Server{..} msg = do
  clientMap <- readTVar clients
  mapM_ (\client -> sendMessage client msg) (Map.elems clientMap)

masterReceiver :: Server -> IO ()
masterReceiver server@Server{..} = forever $ do
  (recv, key) <- NSB.recvFrom messageSocket maxBytes
  let eitherMessage = decode recv
  case eitherMessage of
    Left _        -> logError (printf "From '%s' received non decodable message %s" (show key) (show recv))
    Right message -> messageAssigner server key message
 where
  maxBytes = 1024

messageAssigner :: Server -> ClientKey -> CS.Message -> IO ()
messageAssigner server@Server{..} key message = atomically $ do
  case message of
    CS.WorldMessage worldMessage           -> writeTChan worldChan (convertWithKey worldMessage key)
    CS.ConnectionMessage connectionMessage -> writeTChan connectionChan (convertWithKey connectionMessage key)
    CS.ServiceMessage serviceMessage -> do
      clientM <- lookupClient server key
      case clientM of
        Just client -> do
          writeTChan (serviceChan client) (convert serviceMessage)
        Nothing     -> return ()

-- TODO: use async race (I think) to create a sibling relationship between clientSender and clientServiceConsumer
clientSender :: Server -> Client -> IO ()
clientSender Server{..} Client{..} = forever $ join $ atomically $ do
  message <- readTChan outMessageChan
  return $ do
    NSB.sendTo messageSocket (encode message) key