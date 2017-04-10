{-# LANGUAGE DeriveGeneric #-}

module Common.GTypes (
  ClientName,
  HostName,
  Port,
  Message(..),
  ClientMessage(..)
  ) where

import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Network.Socket (SockAddr)


type ClientName = String

type HostName = String

type Port = String

data Message = ConnectionRequest
             | ConnectionAccepted
             | ConnectionTerminated
             | InvalidName
             | WorldUpdateTest
             deriving (Show, Generic, Eq)

instance Serialize Message

data ClientMessage = ClientMessage SockAddr Message
                   deriving (Show, Generic)