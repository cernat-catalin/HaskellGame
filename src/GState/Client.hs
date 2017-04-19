module GState.Client (
  ConnHandle(..),
  ClientState(..),
  newClientState
  ) where

import Network.Socket (Socket, SockAddr)
import Control.Concurrent.STM (TChan, TVar, newTChanIO, newTVarIO)

import Common.GMessages (WorldMessage, ServiceMessage)
import Common.GObjects (World, newWorld)


data ConnHandle = ConnHandle {
  connSocket :: Socket,
  connAddr   :: SockAddr
} deriving (Show)

data ClientState = ClientState {
  serverHandle    :: ConnHandle,
  worldUpdateChan :: TChan WorldMessage,
  worldInputChan  :: TChan WorldMessage,
  settingsSvcChan :: TChan ServiceMessage,
  world           :: World,
  shouldQuit      :: TVar Bool
}

newClientState :: ConnHandle -> IO ClientState
newClientState connHandle = do
  worldUpdateChan' <- newTChanIO
  worldInputChan'  <- newTChanIO
  shouldQuit'      <- newTVarIO False
  settingsSvcChan' <- newTChanIO

  return ClientState {
    serverHandle    = connHandle,
    worldUpdateChan = worldUpdateChan',
    worldInputChan  = worldInputChan',
    settingsSvcChan = settingsSvcChan',
    world           = newWorld,
    shouldQuit      = shouldQuit'
  }