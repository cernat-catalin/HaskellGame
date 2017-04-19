{-# LANGUAGE RecordWildCards #-}

module GNetwork.Server (
  listenTo,
  masterReceiver,
  sendMessage,
  broadcast
  ) where

import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Data.Map as Map
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (STM, atomically, readTChan, readTVar, writeTChan)
import Control.Monad (forever, join, when)
import Data.Serialize (decode, encode)
import Text.Printf (printf)

import GState.Server (Server(..), Client(..), addClient, removeClient)
import Common.GTypes (Port, ClientSettings(..))
import Common.GMessages (Message(..), ServiceMessage(..), WorldMessage(..), ClientWorldMessage(..))
import GLogger.Server (logInfo, logError)


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
  (recv, addr) <- NSB.recvFrom messageSocket maxBytes
  let eitherMessage = decode recv
  case eitherMessage of
    Right message -> do
      logInfo (printf "Receiver message %s from client %s" (show message) (show addr))
      case message of
        WorldMessage worldMessage -> join $ atomically $ do
          clientMap <- readTVar clients
          let clientM = Map.lookup addr clientMap
          case clientM of
            Just _ -> do
              writeTChan worldChan (ClientWorldMessage addr worldMessage)
              return $ pure ()
            Nothing     -> return $ do
                             logError (printf "Non connection request message from unconnected client %s : %s" (show addr) (show recv))
        -- TODO check if client is in list (make a general check for both service and world messages)
        ServiceMessage serviceMessage -> join $ atomically $ do
          case serviceMessage of
            ConnectionRequest settings -> undefined
            _                          -> do
              clientMap <- readTVar clients
              let clientM = Map.lookup addr clientMap
              case clientM of
                Just client -> do
                  writeTChan (serviceChan client) serviceMessage
                  return $ pure ()
                Nothing     -> return $ do
                                logError (printf "Non connection request message from unconnected client %s : %s" (show addr) (show recv))

        -- ConnectionRequest settings -> initialSetup server addr settings
        -- Quit                       -> do
        --   logInfo (printf "Client %s disconnected" (show addr))
        --   removeClient server addr
        --   atomically $ writeTChan worldMessages (ClientMessage addr RemovePlayer)
    Left _        -> logError (printf "From '%s' received non decodable message %s" (show addr) (show recv))
 where
  maxBytes = 1024

-- clientReceiver :: Client -> IO ()
-- clientReceiver client@Client{..} = join $ atomically $ do
--   message <- readTChan inMessageChan
--   return $ do
--     case message of
--       _ -> logInfo (printf "clientReceiver %s : %s" (show client) (show message))
--     clientReceiver client

-- TODO: use async race (I think) to create a sibling relationship between clientSender and clientReceiver
clientSender :: Server -> Client -> IO ()
clientSender server@Server{..} client@Client{..} = join $ atomically $ do
  message <- readTChan outMessageChan
  return $ do
    NSB.sendTo messageSocket (encode message) key
    when (message /= (ServiceMessage Quit)) $ clientSender server client

initialSetup :: Server -> NS.SockAddr -> ClientSettings -> IO ()
initialSetup server@Server{..} addr settings = join $ atomically $ do
  clientMap <- readTVar clients
  let clientM = Map.lookup addr clientMap
  case clientM of
    Just _  -> return $ do logError (printf "Client %s is already connected but sent a conenction request" (show addr))
    Nothing -> do
      client <- addClient server addr settings
      return $ do
        logInfo (printf "Client %s connected" (show addr))
        -- forkIO (clientReceiver client)
        atomically $ writeTChan worldChan (ClientWorldMessage addr AddPlayer)
        forkIO (clientSender server client)
        return ()

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg =
  writeTChan outMessageChan msg

-- TODO research unbounded STM
broadcast :: Server -> Message -> STM ()
broadcast Server{..} msg = do
  clientMap <- readTVar clients
  mapM_ (\client -> sendMessage client msg) (Map.elems clientMap)