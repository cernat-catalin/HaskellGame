module GState.Client (
  ClientState(..),
  newClientState
  ) where

import Control.Concurrent.STM (TChan, TVar, newTChanIO, newTVarIO)

import GMessages.Client (WorldMessage, PingMessage, SettingsMessage, WorldInputMessage)
import Common.GObjects (World, newWorld)
import Common.GTypes (ClientKey, ConnHandle)



data ClientState = ClientState {
  serverHandle      :: ConnHandle,
  worldUpdateChan   :: TChan WorldMessage,
  worldInputChan    :: TChan WorldInputMessage,
  settingsSvcChan   :: TChan SettingsMessage,
  pingSvcChan       :: TChan PingMessage,
  world             :: World,
  playerKey         :: ClientKey,
  shouldQuit        :: TVar Bool
}

newClientState :: ConnHandle -> ClientKey -> IO ClientState
newClientState connHandle key = do
  worldUpdateChan'   <- newTChanIO
  worldInputChan'    <- newTChanIO
  shouldQuit'        <- newTVarIO False
  settingsSvcChan'   <- newTChanIO
  pingSvcChan'       <- newTChanIO

  return ClientState {
    serverHandle      = connHandle,
    worldUpdateChan   = worldUpdateChan',
    worldInputChan    = worldInputChan',
    settingsSvcChan   = settingsSvcChan',
    pingSvcChan       = pingSvcChan',
    world             = newWorld,
    playerKey         = key,
    shouldQuit        = shouldQuit'
  }