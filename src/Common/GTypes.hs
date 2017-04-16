{-# LANGUAGE DeriveGeneric #-}

module Common.GTypes (
  ClientName,
  HostName,
  Port,
  Message(..),
  ClientMessage(..),
  ClientSettings(..)
  ) where

import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Network.Socket (SockAddr)

import Common.GObjects (World, Player)


type ClientName = String

type HostName = String

type Port = String

data Message = ConnectionRequest ClientSettings
             | ConnectionAccepted
             | ConnectionTerminated
             | WorldUpdate World
             | MoveLeft
             | MoveRight
             | MoveUp
             | MoveDown
             | Quit
             | AddPlayer
             | RemovePlayer
             deriving (Show, Generic, Eq)

data ClientMessage = ClientMessage SockAddr Message

instance Serialize Message

data ClientSettings = ClientSettings {
  name  :: String,
  color :: String
} deriving (Show, Generic, Eq)

instance Serialize ClientSettings