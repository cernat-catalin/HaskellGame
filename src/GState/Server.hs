{-# LANGUAGE RecordWildCards #-}

module GState.Server (
  Client(..),
  Server(..),
  newClient,
  newServer,
  addClient,
  removeClient
  ) where

import qualified Network.Socket as NS
import qualified Control.Concurrent.STM as STM
import qualified Data.Map as Map

import Common.GTypes (Message, ClientMessage, ClientSettings)
import Common.GObjects (World(..), newWorld)


data Client = Client {
  clientAddr     :: NS.SockAddr,
  -- inMessageChan  :: STM.TChan Message,
  outMessageChan :: STM.TChan Message,
  clientSettings :: ClientSettings
}

instance Show Client where
  show client = show (clientAddr client)

data Server = Server {
  clients         :: STM.TVar (Map.Map NS.SockAddr Client),
  messageSocket   :: NS.Socket,
  worldMessages   :: STM.TChan ClientMessage,
  world           :: World
}

newClient :: NS.SockAddr -> ClientSettings -> STM.STM Client
newClient addr settings = do
  outMessageChan <- STM.newTChan
  -- inMessageChan  <- STM.newTChan
  return Client {
    clientAddr     = addr,
    -- inMessageChan  = inMessageChan,
    outMessageChan = outMessageChan,
    clientSettings = settings
  }

newServer :: NS.Socket -> IO Server
newServer messageSocket = do
  clients <- STM.newTVarIO Map.empty
  worldMessages <- STM.newTChanIO
  return Server {
    clients           = clients,
    messageSocket     = messageSocket,
    worldMessages     = worldMessages,
    world             = newWorld
  }

addClient :: Server -> NS.SockAddr -> ClientSettings -> STM.STM Client
addClient Server{..} addr settings = do
  client <- newClient addr settings
  STM.modifyTVar' clients $ Map.insert addr client
  return client

removeClient :: Server -> NS.SockAddr -> IO ()
removeClient Server{..} addr = STM.atomically $ do
  STM.modifyTVar' clients $ Map.delete addr
