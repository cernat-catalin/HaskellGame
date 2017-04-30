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

import GState.Server (Server(..), Client(..), addClient, removeClient, lookupClient)
import Common.GTypes (ClientKey, ClientSettings(..))
import GMessages.Common (Message(..), ConnectionMessage(..), WorldMessage(..), PingMessage(..), ServiceMessage(..))
import GMessages.Server (KeyConnectionMessage(..), KeyWorldMessage(..))
import GLogger.Server (logError, logInfo)
import GNetwork.Server (clientSender, sendMessage)

-- TODO keep an eye on forever $ join $ atomicalliy $ do
connectionService :: Server-> IO ()
connectionService server@Server{..} = forever $ join $ atomically $ do
  KeyConnectionMessage key message <- readTChan connectionChan
  case message of
    ConnectionRequest settings -> do
      clientM <- lookupClient server key
      case clientM of
        Just _  -> return $ logError (printf "Client %s is already connected but sent a conenction request" (show key))
        Nothing -> do
          client <- addClient server key settings
          return $ do
            logInfo (printf "Client %s connected" (show key))
            forkIO (clientReceiver client)
            forkIO (clientSender server client)
            atomically $ writeTChan worldChan (KeyWorldMessage key AddPlayer)
            return ()
      -- TODO: terminate client threads
    ConnectionTerminated -> do
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
    PingMessage pingMessage -> do
      let ping = pingService key
      sendMessage client (ServiceMessage $ PingMessage $ PingResponse ping)
    _ -> return ()