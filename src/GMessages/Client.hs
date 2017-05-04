module GMessages.Client (
  PingMessage(..),
  WorldMessage(..),
  WorldInputMessage(..),
  ServiceMessage(..),
  SettingsMessage(..)
  ) where

import Common.GObjects (World)



data PingMessage = PingRequest
                 | PingResponse String
                 deriving (Show, Eq)

data WorldMessage = WorldUpdate World
                  deriving (Show, Eq)

data WorldInputMessage = MoveLeft
                       | MoveRight
                       | MoveUp
                       | MoveDown
                       deriving (Show, Eq)

data ServiceMessage = PingMessage PingMessage
                    deriving (Show, Eq)

data SettingsMessage = Quit
                     deriving (Show, Eq)