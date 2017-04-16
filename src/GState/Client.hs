module GState.Client (
  ConnHandle(..),
  ClientState(..),
  newClientState
  ) where

import Network.Socket (Socket, SockAddr)
import Control.Concurrent.STM

import Common.GTypes (Message)
import Common.GObjects (World, newWorld)


data ConnHandle = ConnHandle {
  connSocket :: Socket,
  connAddr   :: SockAddr
} deriving (Show)

-- TODO inputMessage doesn't make any sense, rename it
-- Also, it should not be the same data structure as worldMessages
data ClientState = ClientState {
  serverHandle     :: ConnHandle,
  worldMessages    :: TChan Message,
  inputMessages    :: TChan Message,
  servicesMessages :: TChan Message,
  world            :: World,
  shouldQuit       :: TVar Bool
}

newClientState :: ConnHandle -> IO ClientState
newClientState connHandle = do
  worldMessages <- newTChanIO
  inputMessages <- newTChanIO
  shouldQuit <- newTVarIO False
  servicesMessages <- newTChanIO
  return ClientState {
    serverHandle  = connHandle,
    worldMessages = worldMessages,
    inputMessages = inputMessages,
    servicesMessages = servicesMessages,
    world         = newWorld,
    shouldQuit    = shouldQuit
  }