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
import Common.GTypes (Message, ClientMessage)


data Client = Client {
  clientAddr     :: NS.SockAddr,
  outMessageChan :: STM.TChan Message
}

instance Show Client where
  show client = show (clientAddr client)

data Server = Server {
  clients         :: STM.TVar (Map.Map NS.SockAddr Client),
  messageSocket   :: NS.Socket,
  inMessageChan   :: STM.TChan ClientMessage
}

newClient :: NS.SockAddr -> STM.STM Client
newClient addr = do
  outMessageChan <- STM.newTChan
  return Client {
    clientAddr     = addr,
    outMessageChan = outMessageChan
  }

newServer :: NS.Socket -> IO Server
newServer messageSocket = do
  clients <- STM.newTVarIO Map.empty
  inMessageChan <- STM.newTChanIO
  return Server {
    clients           = clients,
    messageSocket     = messageSocket,
    inMessageChan     = inMessageChan
  }

addClient :: Server -> NS.SockAddr -> IO (Maybe Client)
addClient Server{..} addr = STM.atomically $ do
  clientMap <- STM.readTVar clients
  if Map.member addr clientMap
    then return Nothing
    else do client <- newClient addr
            STM.writeTVar clients $ Map.insert addr client clientMap
            return (Just client)

removeClient :: Server -> NS.SockAddr -> IO ()
removeClient Server{..} addr = STM.atomically $ do
  STM.modifyTVar' clients $ Map.delete addr
