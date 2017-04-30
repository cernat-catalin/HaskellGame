{-# LANGUAGE RecordWildCards #-}

module GServices.Client (
  settingsService,
  pingService
  ) where

import Control.Concurrent.STM (atomically, readTChan)
import Control.Concurrent.STM.TVar (modifyTVar')
import Control.Monad (join, forever)
import Data.Serialize (encode)
import Text.Printf (printf)

import GState.Client (ClientState(..))
import GMessages.Common (Message(..), ServiceMessage(..), ConnectionMessage(..), PingMessage(..))
import GMessages.Client (SettingsMessage(..))
import GNetwork.Client (sendMessage)
import GLogger.Client (logInfo)


settingsService :: ClientState -> IO ()
settingsService clientState@ClientState{..} = forever $ join $ atomically $ do
  message <- readTChan settingsSvcChan
  return $ do
    case message of
      Quit -> do
        sendMessage clientState (encode $ ServiceMessage $ ConnectionMessage ConnectionTerminated)
        atomically $ modifyTVar' shouldQuit (const True)
    -- settingsService clientState

pingService :: ClientState -> IO ()
pingService clientState@ClientState{..} = forever $ join $ atomically $ do
  message <- readTChan pingSvcChan
  return $ do
    case message of
      PingRequest -> do
        sendMessage clientState (encode $ ServiceMessage $ PingMessage PingRequest)
        return ()
      PingResponse ping -> do
        logInfo (printf "Ping: %s" ping)