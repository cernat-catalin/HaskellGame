module GState.Client (
  ClientState(..),
  ConnHandle(..),
  newClientState
  ) where

import Network.Socket (Socket, SockAddr)
import Control.Concurrent.STM
import Common.GTypes (Message)


data ConnHandle = ConnHandle {
  connSocket :: Socket,
  connAddr   :: SockAddr
} deriving (Show)

data ClientState = ClientState {
  serverHandle :: ConnHandle,
  serverInChan  :: TChan Message
}

newClientState :: ConnHandle -> IO ClientState
newClientState connHandle = do
  serverChan <- newTChanIO
  return ClientState {
    serverHandle = connHandle,
    serverInChan = serverChan
  }