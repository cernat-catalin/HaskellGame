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

import GState.Server (Server(..), Client(..), addClient)
import Common.GTypes (Port, Message(..), ClientSettings)
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
      case message of
        ConnectionRequest settings -> initialSetup server addr settings
        ConnectionTerminated       -> undefined -- TODO
        _                          -> join $ atomically $ do
          clientMap <- readTVar clients
          let clientM = Map.lookup addr clientMap
          case clientM of
            Just client -> do
              sendMessage client message
              return $ pure ()
            Nothing     -> return $ do
                             logError (printf "Non connection request message from unconnected client %s : %s" (show addr) (show recv))
    Left _        -> logError (printf "From '%s' received non decodable message %s" (show addr) (show recv))
 where
  maxBytes = 1024

clientReceiver :: Client -> IO ()
clientReceiver client@Client{..} = join $ atomically $ do
  message <- readTChan inMessageChan
  return $ do
    case message of
      _ -> logInfo (printf "clientReceiver %s : %s" (show client) (show message))
    clientReceiver client

-- TODO: use async race (I think) to create a sibling relationship between clientSender and clientReceiver
clientSender :: Server -> Client -> IO ()
clientSender server@Server{..} client@Client{..} = join $ atomically $ do
  message <- readTChan outMessageChan
  return $ do
    NSB.sendTo messageSocket (encode message) clientAddr
    when (message /= ConnectionTerminated) $ clientSender server client

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
        forkIO (clientReceiver client)
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