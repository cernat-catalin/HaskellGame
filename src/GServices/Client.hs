{-# LANGUAGE RecordWildCards #-}

module GServices.Client (
  processServicesMessages
  ) where

import Control.Concurrent.STM (atomically, readTChan, writeTChan, isEmptyTChan)
import Control.Concurrent.STM.TVar (modifyTVar')
import Control.Monad (join)
import Data.Serialize (decode, encode)

import GState.Client (ClientState(..))
import GNetwork.Client (sendMessage)
import Common.GTypes (Message(..))


processServicesMessages :: ClientState -> IO ()
processServicesMessages clientState@ClientState{..} = join $ atomically $ do
  message <- readTChan servicesMessages
  return $ do
    case message of
      Quit -> do
        atomically $ modifyTVar' shouldQuit (const True)
        sendMessage clientState (encode message)
        return ()
      _    -> pure ()
    processServicesMessages clientState