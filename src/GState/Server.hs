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

import Network.Socket (Socket, SockAddr)
import qualified Control.Concurrent.STM as STM
import qualified Data.Map as Map

import GCommon.Types.Generic (ClientKey)
import GMessages.Network.ServerClient (Message)
import GMessages.Server (KeyMessage, WorldMessage, ConnectionMessage, PingMessage)
import GCommon.Objects.Objects (World(..), newWorld)



data Client = Client {
  key            :: ClientKey,
  address        :: SockAddr,
  outMessageChan :: STM.TChan Message
}

instance Show Client where
  show client = show (key client)

data Server = Server {
  clients           :: STM.TVar (Map.Map ClientKey Client),
  messageSocket     :: Socket,

  worldChan         :: STM.TChan (KeyMessage WorldMessage),
  connectionSvcChan :: STM.TChan (KeyMessage ConnectionMessage),
  pingSvcChan       :: STM.TChan (KeyMessage PingMessage),

  world             :: World
}

newServer :: Socket -> IO Server
newServer messageSocket = do
  clients'           <- STM.newTVarIO Map.empty
  worldChan'         <- STM.newTChanIO
  connectionSvcChan' <- STM.newTChanIO
  pingSvcChan'       <- STM.newTChanIO

  return Server {
    clients           = clients',
    messageSocket     = messageSocket,
    worldChan         = worldChan',
    connectionSvcChan = connectionSvcChan',
    pingSvcChan       = pingSvcChan',
    world             = newWorld
  }

newClient :: ClientKey -> SockAddr -> STM.STM Client
newClient key' address' = do
  outMessageChan' <- STM.newTChan

  return Client {
    key            = key',
    address        = address',
    outMessageChan = outMessageChan'
  }

addClient :: Server -> ClientKey -> SockAddr -> STM.STM Client
addClient Server{..} key address = do
  client <- newClient key address
  STM.modifyTVar' clients $ Map.insert key client
  return client

removeClient :: Server -> ClientKey -> STM.STM ()
removeClient Server{..} key = do
  STM.modifyTVar' clients $ Map.delete key

lookupClient :: Server -> ClientKey -> STM.STM (Maybe Client)
lookupClient Server{..} key = do
  clientMap <- STM.readTVar clients
  return (Map.lookup key clientMap)
