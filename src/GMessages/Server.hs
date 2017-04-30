module GMessages.Server (
  KeyConnectionMessage(..),
  KeyWorldMessage(..)
  ) where

import Common.GTypes (ClientKey)
import GMessages.Common (ConnectionMessage(..), WorldMessage(..))


data KeyConnectionMessage = KeyConnectionMessage ClientKey ConnectionMessage
                          deriving (Show, Eq)

data KeyWorldMessage = KeyWorldMessage ClientKey WorldMessage
                       deriving (Show, Eq)