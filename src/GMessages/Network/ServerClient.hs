{-# LANGUAGE DeriveGeneric #-}

module GMessages.Network.ServerClient (
  PingMessage(..),
  WorldMessage(..),
  ServiceMessage(..),
  Message(..)
  ) where

import Data.Serialize (Serialize)
import GHC.Generics (Generic)

import Common.GObjects (World)



data PingMessage = PingResponse String
                 deriving (Show, Eq, Generic)

data WorldMessage = WorldUpdate World
                  deriving (Show, Eq, Generic)

data ServiceMessage = PingMessage PingMessage
                    deriving (Show, Eq, Generic)

data Message = WorldMessage WorldMessage
             | ServiceMessage ServiceMessage
             deriving (Show, Eq, Generic)

instance Serialize PingMessage
instance Serialize ServiceMessage
instance Serialize WorldMessage
instance Serialize Message