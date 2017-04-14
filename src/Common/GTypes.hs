{-# LANGUAGE DeriveGeneric #-}

module Common.GTypes (
  ClientName,
  HostName,
  Port,
  Message(..),
  ClientSettings(..)
  ) where

import Data.Serialize (Serialize)
import GHC.Generics (Generic)


type ClientName = String

type HostName = String

type Port = String

data Message = ConnectionRequest ClientSettings
             | ConnectionAccepted
             | ConnectionTerminated
             | WorldUpdateTest
             deriving (Show, Generic, Eq)

instance Serialize Message

data ClientSettings = ClientSettings {
  name  :: String,
  color :: String
} deriving (Show, Generic, Eq)

instance Serialize ClientSettings