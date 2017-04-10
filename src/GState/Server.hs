{-# LANGUAGE RecordWildCards #-}

module GState.Server (
  Client(..),
  Server(..),
  newServer,
  newClient,
  addClient,
  removeClient
  ) where

import           Network.Socket (Socket, SockAddr)
import           Control.Concurrent.STM
import qualified Data.Map as Map
import           Common.GTypes (Message, ClientMessage)


-- TODO: do I really need a TChan here ? 
data Client = Client {
  clientAddr     :: SockAddr,
  outMessageChan :: TChan Message
}

instance Show Client where
  show client = show (clientAddr client)

-- TODO: do I really need a TChan here ? YES
data Server = Server {
  clients         :: TVar (Map.Map SockAddr Client),
  messageSocket   :: Socket,
  inMessageChan   :: TChan ClientMessage
}

newServer :: Socket -> IO Server
newServer messageSocket = do
  clients     <- newTVarIO Map.empty
  inMessageChan <- newTChanIO
  return Server {
    clients           = clients,
    messageSocket     = messageSocket,
    inMessageChan     = inMessageChan
  }

newClient :: SockAddr -> STM Client
newClient addr = do
  outMessageChan <- newTChan
  return Client {
    clientAddr     = addr,
    outMessageChan = outMessageChan
  }

removeClient :: Server -> SockAddr -> IO ()
removeClient Server{..} addr = atomically $ do
  modifyTVar' clients $ Map.delete addr

addClient :: Server -> SockAddr -> IO (Maybe Client)
addClient Server{..} addr = atomically $ do
  clientMap <- readTVar clients
  if Map.member addr clientMap
    then return Nothing
    else do client <- newClient addr
            writeTVar clients $ Map.insert addr client clientMap
            return (Just client)