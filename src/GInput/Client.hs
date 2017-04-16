{-# LANGUAGE RecordWildCards #-}

module GInput.Client (
  keyCallback,
  processInputMessages
  ) where

import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent.STM (atomically, readTChan, writeTChan, isEmptyTChan)
import Control.Monad (join)
import Data.Serialize (decode, encode)
import Text.Printf (printf)

import GState.Client (ClientState(..))
import Common.GTypes (Message(..))
import GNetwork.Client (sendMessage)
import GLogger.Client (logInfo)


keyCallback :: ClientState -> GLFW.KeyCallback
keyCallback ClientState{..} _ key _  keyState _ =
  case keyState of
    GLFW.KeyState'Pressed ->
      case key of
        GLFW.Key'A      -> atomically $ writeTChan inputMessages MoveLeft
        GLFW.Key'D      -> atomically $ writeTChan inputMessages MoveRight
        GLFW.Key'W      -> atomically $ writeTChan inputMessages MoveUp
        GLFW.Key'S      -> atomically $ writeTChan inputMessages MoveDown
        GLFW.Key'Escape      -> atomically $ writeTChan servicesMessages Quit
        _               -> return ()
    _                     -> return ()

processInputMessages :: ClientState -> IO ()
processInputMessages clientState@ClientState{..} = join $ atomically $ do
  emptyChan <- isEmptyTChan inputMessages
  if not emptyChan
    then do
        message <- readTChan inputMessages
        return $ do
          sendMessage clientState (encode message)
          processInputMessages clientState
    else do
      return $ pure ()