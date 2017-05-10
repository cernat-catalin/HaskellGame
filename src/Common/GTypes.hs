{-# LANGUAGE DeriveGeneric #-}

module Common.GTypes (
  ClientKey,
  HostName,
  Port,
  Point,
  Radius,
  ClientSettings(..),
  ConnHandle(..)
  ) where

import Graphics.Rendering.OpenGL (GLdouble)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Network.Socket (Socket, SockAddr)


type ClientKey  = SockAddr
type HostName   = String
type Port       = String
type Point      = (GLdouble, GLdouble)
type Radius     = GLdouble

data ClientSettings = ClientSettings {
  name  :: String,
  color :: String
} deriving (Show, Generic, Eq)

data ConnHandle = ConnHandle {
  connSocket :: Socket,
  connAddr   :: SockAddr
} deriving (Show)

instance Serialize ClientSettings