{-# LANGUAGE RecordWildCards #-}

module GFunc.Client.Setup (
  initialSetup
  ) where

import qualified Network.Socket.ByteString as NSB
import Data.Serialize (decode)
import Text.Printf (printf)

import GLogger.Client (logInfo)
import Common.GTypes (ClientSettings(..), ConnHandle(..), ClientKey)
import GState.Client (ClientState(..), newClientState)
import GNetwork.Client (sendMessage)
import GMessages.Network.ClientServer (Message(..), ConnectionMessage(..))
import qualified GMessages.Network.ServerClient as SC



initialSetup :: ConnHandle -> IO ClientState
initialSetup connHandle = do
  let settings = ClientSettings {name = "Levi", color = "Green"}
  _   <- sendMessage connHandle (ConnectionMessage $ ConnectionRequest settings)
  key <- receivePlayerKey connHandle
  logInfo (printf "Key received: %s" (show key))
  newClientState connHandle key

receivePlayerKey :: ConnHandle -> IO ClientKey
receivePlayerKey connHandle@ConnHandle{..} = do
  recv <- NSB.recv connSocket maxBytes
  let eitherMessage = decode recv
  case eitherMessage of
    Right message -> case message of
      SC.PlayerKey key -> return key
    Left _        -> receivePlayerKey connHandle
 where
  maxBytes = 1024