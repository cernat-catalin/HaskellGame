{-# LANGUAGE RecordWildCards #-}

module GServices.Client (
  settingsService,
  pingService
  ) where

import Control.Concurrent.STM (atomically, readTChan)
import Control.Concurrent.STM.TVar (modifyTVar')
import Control.Monad (join, forever)
import Text.Printf (printf)

import GState.Client (ClientState(..))
import GMessages.Network.ClientServer as CS (Message(..), ServiceMessage(..), ConnectionMessage(..), PingMessage(..))
import GMessages.Client as C (SettingsMessage(..), PingMessage(..))
import GNetwork.Client (sendMessage)
import GLogger.Client (logInfo)



settingsService :: ClientState -> IO ()
settingsService ClientState{..} = forever $ join $ atomically $ do
  message <- readTChan settingsSvcChan
  return $ do
    case message of
      Quit -> do
        _ <- sendMessage serverHandle (ConnectionMessage ConnectionTerminated)
        atomically $ modifyTVar' shouldQuit (const True)

pingService :: ClientState -> IO ()
pingService ClientState{..} = forever $ join $ atomically $ do
  message <- readTChan pingSvcChan
  return $ do
    case message of
      C.PingRequest -> do
        _ <- sendMessage serverHandle (ServiceMessage $ PingMessage CS.PingRequest)
        return ()
      PingResponse ping -> do
        logInfo (printf "Ping: %s" ping)