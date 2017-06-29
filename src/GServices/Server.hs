{-# LANGUAGE RecordWildCards #-}

module GServices.Server (
  connectionService,
  pingService
  ) where

import Text.Printf (printf)
import Control.Concurrent.STM (atomically, readTChan, writeTChan)
import Control.Monad (join, forever)
import Control.Concurrent (forkIO, threadDelay)
import Data.Serialize (encode)

import GMessages.Server as S (KeyMessage(..), ConnectionMessage(..), ServiceMessage(..), PingMessage(..), WorldMessage(..))
import GMessages.Network.ServerClient as SC (PingMessage(..), ServiceMessage(..), Message(..), ConnectionMessage(..))
import GState.Server (Server(..), Client(..), addClient, removeClient, lookupClient)
import GCommon.Types.Generic (ClientKey)
import GLogger.Server (logError, logInfo)
import GNetwork.Server (clientSender, sendMessage, sendMessageRaw)



connectionService :: Server-> IO ()
connectionService server@Server{..} = forever $ join $ atomically $ do
  (KeyMessage key message) <- readTChan connectionSvcChan
  case message of
    ConnectionRequest settings -> do
      clientM <- lookupClient server key
      case clientM of
        Just _  -> return $ logError (printf "Client %s is already connected but sent a conenction request" (show key))
        Nothing -> do
          client <- addClient server key
          writeTChan worldChan (KeyMessage key $ AddPlayer settings)
          return $ do
            logInfo (printf "Client %s connected" (show key))
            _ <- forkIO (clientSender server client)
            sendMessageRaw server client (encode (SC.PlayerKey key))
            return ()
      -- TODO: terminate client threads
    ConnectionTerminated -> do
      clientM <- lookupClient server key
      case clientM of
        Nothing -> return $ logError (printf "Client %s is not connected but requested a disconnect" (show key))
        Just _ -> do
          removeClient server key
          writeTChan worldChan (KeyMessage key RemovePlayer)
          return $ logInfo (printf "Client %s disconnected" (show key))

pingService :: Server -> IO ()
pingService server@Server{..} = forever $ join $ atomically $ do
  (KeyMessage key message) <- readTChan pingSvcChan
  clientM <- lookupClient server key
  return $ do
    case clientM of
      Nothing     -> return ()
      Just client -> case message of
        PingRequest ping -> do
          atomically $ sendMessage client (SC.ServiceMessage $ SC.PingMessage $ SC.PingResponse ping)