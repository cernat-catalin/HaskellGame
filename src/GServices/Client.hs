{-# LANGUAGE RecordWildCards #-}

module GServices.Client (
  settingsService
  ) where

import Control.Concurrent.STM (atomically, readTChan)
import Control.Concurrent.STM.TVar (modifyTVar')
import Control.Monad (join)
import Data.Serialize (encode)

import GState.Client (ClientState(..))
import Common.GMessages (ServiceMessage(..))
import GNetwork.Client (sendMessage)


settingsService :: ClientState -> IO ()
settingsService clientState@ClientState{..} = join $ atomically $ do
  message <- readTChan settingsSvcChan
  return $ do
    case message of
      Quit -> do
        sendMessage clientState (encode message)
        atomically $ modifyTVar' shouldQuit (const True)
      _    -> pure ()
    settingsService clientState