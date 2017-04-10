{-# LANGUAGE DeriveGeneric #-}

module Common.GTypes (
  ClientName,
  Message(..),
  ClientMessage(..),
  HostName,
  Port
  ) where

import Data.Serialize (Serialize)
import GHC.Generics   (Generic)
import Network.Socket (SockAddr)


type ClientName = String

type HostName = String

type Port = String

data Message = ConnectionRequest
             | ConnectionAccepted
             | InvalidName
             | WorldUpdateTest
             deriving (Show, Generic)

instance Serialize Message

data ClientMessage = ClientMessage SockAddr Message
                   deriving (Show, Generic)