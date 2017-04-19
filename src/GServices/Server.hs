module GServices.Server (
  connectionService
  ) where

import GState.Server (Server(..))
import Common.GTypes (ClientKey, ClientSettings(..))

connectionService :: Server -> ClientKey -> IO ()
connectionService = undefined