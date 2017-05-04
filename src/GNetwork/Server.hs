{-# LANGUAGE RecordWildCards #-}

module GNetwork.Server (
  listenTo,
  masterReceiver,
  sendMessage,
  broadcast,
  clientSender
  ) where

import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Data.Map as Map
import Control.Concurrent.STM (STM, atomically, readTChan, readTVar, writeTChan)
import Control.Monad (forever, join)
import Data.Serialize (decode, encode)
import Text.Printf (printf)

import GState.Server (Server(..), Client(..), lookupClient)
import Common.GTypes (Port, ClientKey)
import GLogger.Server (logError)
import GMessages.Network.Converter (convertWithKey)
import GMessages.Network.ClientServer as CS (Message(..)) 
import GMessages.Network.ServerClient as SC (Message)
import GMessages.Server as S (ServiceMessage(..))



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

masterReceiver :: Server -> IO ()
masterReceiver server@Server{..} = forever $ do
  (recv, key) <- NSB.recvFrom messageSocket maxBytes
  let eitherMessage = decode recv

  case eitherMessage of
    Left _        -> logError (printf "From '%s' received non decodable message %s" (show key) (show recv))

    Right message -> do
      case message of
        WorldMessage worldMessage     -> atomically $ writeTChan worldChan (convertWithKey worldMessage key)
        ServiceMessage serviceMessage -> case (convertWithKey serviceMessage key) of
          S.ConnectionMessage connectionMessage -> atomically $ writeTChan connectionChan connectionMessage
          _ -> writeToServiceChan server key (convertWithKey serviceMessage key)
 where
  maxBytes = 1024

-- TODO: use async race (I think) to create a sibling relationship between clientSender and clientReceiver

clientSender :: Server -> Client -> IO ()
clientSender Server{..} Client{..} = forever $ join $ atomically $ do
  message <- readTChan outMessageChan
  return $ do
    NSB.sendTo messageSocket (encode message) key

sendMessage :: Client -> SC.Message -> STM ()
sendMessage Client{..} msg =
  writeTChan outMessageChan msg

-- TODO research unbounded STM
broadcast :: Server -> SC.Message -> STM ()
broadcast Server{..} msg = do
  clientMap <- readTVar clients
  mapM_ (\client -> sendMessage client msg) (Map.elems clientMap)

writeToServiceChan :: Server -> ClientKey -> S.ServiceMessage -> IO ()
writeToServiceChan server@Server{..} key message = join $ atomically $ do
  clientM <- lookupClient server key
  case clientM of
    Just client -> do
      writeTChan (serviceChan client) message
      return $ pure ()
    Nothing     -> return $ logError (printf "Non connection request message from unconnected client %s : %s" (show key) (show message))