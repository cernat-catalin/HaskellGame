{-# LANGUAGE RecordWildCards #-}

module GState.Server (
  Client(..),
  Server(..),
  newServer,
  newClient,
  addClient,
  removeClient
  ) where

import Network.Socket (Socket)
import Control.Concurrent.STM
import qualified Data.Map as Map
import Common.GTypes (ClientName, Message)


data Client = Client {
  clientName :: ClientName,
  clientSocket :: Socket,
  clientSendChan :: TChan Message
}

data Server = Server {
  clients :: TVar (Map.Map ClientName Client)
}

newServer :: IO Server
newServer = do
  clients <- newTVarIO Map.empty
  return Server { clients = clients }

newClient :: ClientName -> Socket -> STM Client
newClient name sock = do
  chan <- newTChan
  return Client {
    clientName     = name,
    clientSocket   = sock,
    clientSendChan = chan
  }

removeClient :: Server -> ClientName -> IO ()
removeClient Server{..} name = atomically $ do
  modifyTVar' clients $ Map.delete name

addClient :: Server -> ClientName -> Socket -> IO (Maybe Client)
addClient Server{..} name sock = atomically $ do
  clientMap <- readTVar clients
  if Map.member name clientMap
    then return Nothing
    else do client <- newClient name sock
            writeTVar clients $ Map.insert name client clientMap
            return (Just client)