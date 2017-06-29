module GState.Client (
  KeysState(..),
  ClientState(..),
  newClientState
  ) where

import Control.Concurrent.STM (TChan, TVar, newTChanIO, newTVarIO)
import Data.Time.Clock (UTCTime(..), getCurrentTime)

import GMessages.Client (WorldMessage, PingMessage, SettingsMessage, WorldInputMessage)
import qualified GMessages.Network.ClientServer as CS
import GCommon.Objects.Objects (World, newWorld)
import GCommon.Types.Generic (ClientKey, ConnHandle)
import GOpenGL.Meshes (Mesh)


data KeysState = KeysState {
  up    :: Bool,
  left  :: Bool,
  down  :: Bool,
  right :: Bool,
  space :: Bool
}

newKeysState :: KeysState
newKeysState = KeysState False False False False False

data ClientState = ClientState {
  serverHandle      :: ConnHandle,

  worldUpdateChan   :: TChan WorldMessage,
  worldInputChan    :: TChan WorldInputMessage,
  serverOutChan     :: TChan CS.Message,
  settingsSvcChan   :: TChan SettingsMessage,
  pingSvcChan       :: TChan PingMessage,

  world             :: World,
  playerKey         :: ClientKey,
  keysState         :: KeysState,
  shouldQuit        :: TVar Bool,
  menuIsOn          :: TVar Bool,
  lastTimeShot      :: TVar UTCTime
}

newClientState :: ConnHandle -> ClientKey -> IO ClientState
newClientState connHandle key = do
  worldUpdateChan'   <- newTChanIO
  worldInputChan'    <- newTChanIO
  serverOutChan'     <- newTChanIO
  shouldQuit'        <- newTVarIO False
  menuIsOn'          <- newTVarIO False
  settingsSvcChan'   <- newTChanIO
  pingSvcChan'       <- newTChanIO
  time               <- getCurrentTime
  lastTimeShot'      <- newTVarIO time

  return ClientState {
    serverHandle      = connHandle,
    worldUpdateChan   = worldUpdateChan',
    worldInputChan    = worldInputChan',
    serverOutChan     = serverOutChan',
    settingsSvcChan   = settingsSvcChan',
    pingSvcChan       = pingSvcChan',
    world             = newWorld,
    playerKey         = key,
    keysState         = newKeysState,
    shouldQuit        = shouldQuit',
    menuIsOn          = menuIsOn',
    lastTimeShot      = lastTimeShot'
  }