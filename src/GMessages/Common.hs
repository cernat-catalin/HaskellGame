{-# LANGUAGE DeriveGeneric #-}

module GMessages.Common (
  ConnectionMessage(..),
  PingMessage(..),
  ServiceMessage(..),
  WorldMessage(..),
  Message(..)
  ) where

import Data.Serialize (Serialize)
import GHC.Generics (Generic)

import Common.GTypes (ClientSettings, ClientKey)
import Common.GObjects (World)


data ConnectionMessage = ConnectionRequest ClientSettings
                       | ConnectionTerminated
                       deriving (Show, Eq, Generic)

instance Serialize ConnectionMessage


data PingMessage = PingRequest
                 | PingResponse String
                 deriving (Show, Eq, Generic)

instance Serialize PingMessage


data ServiceMessage = ConnectionMessage ConnectionMessage
                    | PingMessage PingMessage
                    deriving (Show, Eq, Generic)

instance Serialize ServiceMessage


data WorldMessage = WorldUpdate World
                  | MoveLeft
                  | MoveRight
                  | MoveUp
                  | MoveDown
                  | AddPlayer
                  | RemovePlayer
                  deriving (Show, Eq, Generic)

instance Serialize WorldMessage


data Message = WorldMessage WorldMessage
             | ServiceMessage ServiceMessage
             deriving (Show, Eq, Generic)

instance Serialize Message