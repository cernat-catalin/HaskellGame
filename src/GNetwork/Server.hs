{-# LANGUAGE RecordWildCards #-}

module GNetwork.Server (
  sendMessage,
  broadcast
  ) where

import Control.Concurrent.STM
import GState.Server
import Common.GTypes
import qualified Data.Map as Map


sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg =
  writeTChan clientSendChan msg

broadcast :: Server -> Message -> STM ()
broadcast Server{..} msg = do
  clientMap <- readTVar clients
  mapM_ (\client -> sendMessage client msg) (Map.elems clientMap)