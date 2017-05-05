{-# LANGUAGE RecordWildCards #-}

module GState.Server (
  Client(..),
  Server(..),
  newClient,
  newServer,
  addClient,
  removeClient,
  lookupClient
  ) where

import Network.Socket (Socket)
import qualified Control.Concurrent.STM as STM
import qualified Data.Map as Map

import Common.GTypes (ClientKey, ClientSettings)
import GMessages.Network.ServerClient (Message)
import GMessages.Server (KeyMessage, WorldMessage, ConnectionMessage, ServiceMessage)
import Common.GObjects (World(..), newWorld)



data Client = Client {
  key            :: ClientKey,
  serviceChan    :: STM.TChan ServiceMessage,
  outMessageChan :: STM.TChan Message,
  settings       :: ClientSettings
}

instance Show Client where
  show client = show (key client) ++ show (settings client)

data Server = Server {
  clients        :: STM.TVar (Map.Map ClientKey Client),
  messageSocket  :: Socket,
  worldChan      :: STM.TChan (KeyMessage WorldMessage),
  connectionChan :: STM.TChan (KeyMessage ConnectionMessage),
  world          :: World
}

newServer :: Socket -> IO Server
newServer messageSocket = do
  clients'        <- STM.newTVarIO Map.empty
  worldChan'      <- STM.newTChanIO
  connectionChan' <- STM.newTChanIO

  return Server {
    clients        = clients',
    messageSocket  = messageSocket,
    worldChan      = worldChan',
    connectionChan = connectionChan',
    world          = newWorld
  }

newClient :: ClientKey -> ClientSettings -> STM.STM Client
newClient key' settings' = do
  serviceChan'    <- STM.newTChan
  outMessageChan' <- STM.newTChan

  return Client {
    key            = key',
    serviceChan    = serviceChan',
    outMessageChan = outMessageChan',
    settings       = settings'
  }

addClient :: Server -> ClientKey -> ClientSettings -> STM.STM Client
addClient Server{..} key settings = do
  client <- newClient key settings
  STM.modifyTVar' clients $ Map.insert key client
  return client

removeClient :: Server -> ClientKey -> STM.STM ()
removeClient Server{..} key = do
  STM.modifyTVar' clients $ Map.delete key

lookupClient :: Server -> ClientKey -> STM.STM (Maybe Client)
lookupClient Server{..} key = do
  clientMap <- STM.readTVar clients
  return (Map.lookup key clientMap)
