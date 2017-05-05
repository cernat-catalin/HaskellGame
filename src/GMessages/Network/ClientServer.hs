{-# LANGUAGE DeriveGeneric #-}

module GMessages.Network.ClientServer (
  ConnectionMessage(..),
  PingMessage(..),
  WorldMessage(..),
  ServiceMessage(..),
  Message(..)
  ) where

import Data.Serialize (Serialize)
import GHC.Generics (Generic)

import Common.GTypes (ClientSettings, Point)



data ConnectionMessage = ConnectionRequest ClientSettings
                       | ConnectionTerminated
                       deriving (Show, Eq, Generic)

data PingMessage = PingRequest
                 deriving (Show, Eq, Generic)

data WorldMessage = PositionUpdate Point
                  deriving (Show, Eq, Generic)

data ServiceMessage = PingMessage PingMessage
                    deriving (Show, Eq, Generic)

data Message = ConnectionMessage ConnectionMessage
             | WorldMessage WorldMessage
             | ServiceMessage ServiceMessage
             deriving (Show, Eq, Generic)

instance Serialize ConnectionMessage
instance Serialize PingMessage
instance Serialize WorldMessage
instance Serialize ServiceMessage
instance Serialize Message