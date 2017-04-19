{-# LANGUAGE DeriveGeneric #-}

module Common.GMessages (
  ServiceMessage(..),
  WorldMessage(..),
  Message(..),
  ClientWorldMessage(..),
  ) where

import Data.Serialize (Serialize)
import GHC.Generics (Generic)

import Common.GTypes (ClientSettings, ClientKey)
import Common.GObjects (World)


data ServiceMessage = ConnectionRequest ClientSettings
                    | ConnectionTerminated
                    | Quit
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

data ClientWorldMessage = ClientWorldMessage ClientKey WorldMessage