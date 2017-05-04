{-# LANGUAGE RecordWildCards #-}

module GServices.Server (
  connectionService,
  pingService
  ) where

import Text.Printf (printf)
import Control.Concurrent.STM (atomically, readTChan, readTVar, writeTChan)
import Control.Monad (join, forever)
import Control.Concurrent (forkIO)
import Data.Serialize (decode, encode)

import GMessages.Server as S (ConnectionMessage(..), ServiceMessage(..), PingMessage(..), WorldMessage(..))
import GMessages.Network.ServerClient as SC (PingMessage(..), ServiceMessage(..), Message(..))
import GState.Server (Server(..), Client(..), addClient, removeClient, lookupClient)
import Common.GTypes (ClientKey, ClientSettings(..))
import GLogger.Server (logError, logInfo)
import GNetwork.Server (clientSender, sendMessage)

-- TODO keep an eye on forever $ join $ atomicalliy $ do
connectionService :: Server-> IO ()
connectionService server@Server{..} = forever $ join $ atomically $ do
  message <- readTChan connectionChan
  case message of
    ConnectionRequest key settings -> do
      clientM <- lookupClient server key
      case clientM of
        Just _  -> return $ logError (printf "Client %s is already connected but sent a conenction request" (show key))
        Nothing -> do
          client <- addClient server key settings
          return $ do
            logInfo (printf "Client %s connected" (show key))
            forkIO (clientReceiver client)
            forkIO (clientSender server client)
            atomically $ writeTChan worldChan (AddPlayer key)
            return ()
      -- TODO: terminate client threads
    ConnectionTerminated key -> do
      clientM <- lookupClient server key
      case clientM of
        Nothing -> return $ logError (printf "Client %s is not connected but requested a disconnect" (show key))
        Just client -> do
          removeClient server key
          return $ pure ()

pingService :: ClientKey -> String
pingService _ = "Ping is 123ms"


-- TODO: rename this
clientReceiver :: Client -> IO ()
clientReceiver client@Client{..} = forever $ atomically $ do
  message <- readTChan serviceChan
  case message of
    S.PingMessage pingMessage ->
      case pingMessage of
        PingRequest key -> do
          let ping = pingService key ++ "!"
          sendMessage client (SC.ServiceMessage $ SC.PingMessage $ PingResponse ping)
    _ -> return ()