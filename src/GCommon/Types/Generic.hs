{-# LANGUAGE DeriveGeneric #-}

module GCommon.Types.Generic (
  ClientKey,
  HostName,
  Port,
  ClientSettings(..),
  ConnHandle(..)
  ) where

import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Network.Socket (Socket, SockAddr)


type ClientKey  = SockAddr
type HostName   = String
type Port       = String

data ClientSettings = ClientSettings {
  name  :: String,
  color :: String
} deriving (Show, Generic, Eq)

data ConnHandle = ConnHandle {
  connSocket :: Socket,
  connAddr   :: SockAddr
} deriving (Show)

instance Serialize ClientSettings