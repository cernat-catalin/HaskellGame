{-# LANGUAGE RecordWildCards #-}

module GInput.Client (
  keyCallback,
  processWorldInput
  ) where

import Control.Concurrent.STM (atomically, readTChan, writeTChan, isEmptyTChan)
import Control.Monad (join)
import qualified Graphics.UI.GLFW as GLFW

import GState.Client (ClientState(..))
import GMessages.Client (SettingsMessage(..), WorldInputMessage(..), PingMessage(..))



-- TODO client services (quit)
keyCallback :: ClientState -> GLFW.KeyCallback
keyCallback ClientState{..} _ key _  keyState _ =
  case keyState of
    GLFW.KeyState'Pressed ->
      case key of
        GLFW.Key'A      -> atomically $ writeTChan worldInputChan MoveLeft
        GLFW.Key'D      -> atomically $ writeTChan worldInputChan MoveRight
        GLFW.Key'W      -> atomically $ writeTChan worldInputChan MoveUp
        GLFW.Key'S      -> atomically $ writeTChan worldInputChan MoveDown
        GLFW.Key'Escape -> atomically $ writeTChan settingsSvcChan Quit
        GLFW.Key'P      -> atomically $ writeTChan pingSvcChan PingRequest
        _               -> return ()
    _                     -> return ()

processWorldInput :: ClientState -> IO ()
processWorldInput clientState@ClientState{..} = join $ atomically $ do
  emptyChan <- isEmptyTChan worldInputChan
  if not emptyChan
    then do
        _ <- readTChan worldInputChan
        return $ do
          -- sendMessage clientState (encode $ WorldMessage message)
          processWorldInput clientState
    else do
      return $ pure ()