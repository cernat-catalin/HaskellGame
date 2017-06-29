{-# LANGUAGE DeriveGeneric #-}

module GCommon.Types.Generic (
  ClientKey,
  HostName,
  Port,
  PlayerSettings(..),
  ConnHandle(..),
  Direction(..)
  ) where

import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Network.Socket (Socket, SockAddr(..))


type ClientKey  = SockAddr
type HostName   = String
type Port       = String


data PlayerSettings = PlayerSettings {
  name      :: String,
  team      :: Int,
  vehicleId :: Int
} deriving (Show, Generic, Eq)

data ConnHandle = ConnHandle {
  connSocket :: Socket,
  connAddr   :: SockAddr
} deriving (Show)

data Direction = DUp | DRight | DDown | DLeft

instance Serialize PlayerSettings