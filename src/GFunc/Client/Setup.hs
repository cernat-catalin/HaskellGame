module GFunc.Client.Setup (
  initialSetup
  ) where

import Common.GTypes (ClientSettings(..))
import GState.Client (ClientState(..))
import GNetwork.Client (sendMessage)
import GMessages.Network.ClientServer (Message(..), ConnectionMessage(..))

initialSetup :: ClientState -> IO ()
initialSetup clientState = do
  let settings = ClientSettings {name = "Levi", color = "Green"}
  _ <- sendMessage clientState (ConnectionMessage $ ConnectionRequest settings)
  return ()