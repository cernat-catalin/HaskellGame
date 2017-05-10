{-# LANGUAGE RecordWildCards #-}

module GServices.Server (
  connectionService,
  pingService
  ) where

import Text.Printf (printf)
import Control.Concurrent.STM (atomically, readTChan, writeTChan)
import Control.Monad (join, forever)
import Control.Concurrent (forkIO)
import Data.Serialize (encode)

import GMessages.Server as S (KeyMessage(..), ConnectionMessage(..), ServiceMessage(..), PingMessage(..), WorldMessage(..))
import GMessages.Network.ServerClient as SC (PingMessage(..), ServiceMessage(..), Message(..), ConnectionMessage(..))
import GState.Server (Server(..), Client(..), addClient, removeClient, lookupClient)
import Common.GTypes (ClientKey)
import GLogger.Server (logError, logInfo)
import GNetwork.Server (clientSender, sendMessage, sendMessageRaw)



connectionService :: Server-> IO ()
connectionService server@Server{..} = forever $ join $ atomically $ do
  (KeyMessage key message) <- readTChan connectionChan
  case message of
    ConnectionRequest settings -> do
      clientM <- lookupClient server key
      case clientM of
        Just _  -> return $ logError (printf "Client %s is already connected but sent a conenction request" (show key))
        Nothing -> do
          client <- addClient server key settings
          return $ do
            logInfo (printf "Client %s connected" (show key))
            _ <- forkIO (clientServiceConsumer client)
            _ <- forkIO (clientSender server client)
            sendMessageRaw server client (encode (SC.PlayerKey key))
            atomically $ writeTChan worldChan (KeyMessage key AddPlayer)
            return ()
      -- TODO: terminate client threads
    ConnectionTerminated -> do
      clientM <- lookupClient server key
      case clientM of
        Nothing -> return $ logError (printf "Client %s is not connected but requested a disconnect" (show key))
        Just _ -> do
          removeClient server key
          return $ logInfo (printf "Client %s disconnected" (show key))

pingService :: ClientKey -> String
pingService _ = "Ping is 123ms"

clientServiceConsumer :: Client -> IO ()
clientServiceConsumer client@Client{..} = forever $ atomically $ do
  message <- readTChan serviceChan
  case message of
    S.PingMessage pingMessage ->
      case pingMessage of
        PingRequest -> do
          let ping = pingService key ++ "!"
          _ <- sendMessage client (SC.ServiceMessage $ SC.PingMessage $ PingResponse ping)
          return ()