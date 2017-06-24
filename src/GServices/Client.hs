{-# LANGUAGE RecordWildCards #-}

module GServices.Client (
  settingsService,
  pingService,
  serverOutService,
  menuService
  ) where

import Control.Concurrent.STM (atomically, readTChan, readTVar, retry)
import Control.Concurrent.STM.TVar (modifyTVar')
import Control.Monad (join, forever)
import Text.Printf (printf)

import GFunc.Client.Setup (gatherSettings)
import GState.Client (ClientState(..))
import GMessages.Network.ClientServer as CS (Message(..), ServiceMessage(..), ConnectionMessage(..), PingMessage(..), WorldMessage(..))
import GMessages.Client as C (SettingsMessage(..), PingMessage(..))
import GNetwork.Client (sendMessage)
import GLogger.Client (logInfo)



settingsService :: ClientState -> IO ()
settingsService ClientState{..} = forever $ join $ atomically $ do
  message <- readTChan settingsSvcChan
  return $ do
    case message of
      Quit       -> do
        _ <- sendMessage serverHandle (ServiceMessage $ ConnectionMessage ConnectionTerminated)
        atomically $ modifyTVar' shouldQuit (const True)
      OpenMenu   -> do
        atomically $ modifyTVar' menuIsOn (const True)

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


serverOutService :: ClientState -> IO ()
serverOutService ClientState{..} = forever $ join $ atomically $ do
  message <- readTChan serverOutChan
  return $ do
    _ <- sendMessage serverHandle message
    return ()

menuService :: ClientState -> IO ()
menuService ClientState{..} = forever $ join $ atomically $ do
  menuIsOn_ <- readTVar menuIsOn
  if menuIsOn_ == True
    then return $ do
      settings <- gatherSettings
      _ <- sendMessage serverHandle (WorldMessage $ SettingsUpdate settings)
      atomically $ modifyTVar' menuIsOn (const False)
      return ()
    else retry
