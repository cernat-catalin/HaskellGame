{-# LANGUAGE RecordWildCards #-}

module GInput.Client (
  keyCallback,
  processWorldInput
  ) where

import Control.Concurrent.STM (atomically, writeTChan, readTChan, isEmptyTChan, readTVar, writeTVar)
import qualified Graphics.UI.GLFW as GLFW
import Control.Monad (join, unless)

import GCommon.Objects.Objects as GO
import GState.Client (ClientState(..), KeysState(..))
import GMessages.Client (SettingsMessage(..), WorldInputMessage(..), PingMessage(..))
import qualified GMessages.Network.ClientServer as CS



-- TODO client services (quit)
keyCallback :: ClientState -> GLFW.KeyCallback
keyCallback clientState@ClientState{..} _ key _  keyState _ =
  case keyState of
    GLFW.KeyState'Pressed  ->
      case key of
        GLFW.Key'A      -> atomically $ writeTChan worldInputChan PressLeft
        GLFW.Key'D      -> atomically $ writeTChan worldInputChan PressRight
        GLFW.Key'W      -> atomically $ writeTChan worldInputChan PressUp
        GLFW.Key'S      -> atomically $ writeTChan worldInputChan PressDown
        GLFW.Key'Escape -> atomically $ writeTChan settingsSvcChan Quit
        GLFW.Key'M      -> atomically $ writeTChan settingsSvcChan OpenMenu
        GLFW.Key'P      -> atomically $ writeTChan pingSvcChan PingRequest
        GLFW.Key'Space  -> atomically $ writeTChan worldInputChan PressSpace
        _               -> return ()
    GLFW.KeyState'Released ->
      case key of
        GLFW.Key'A      -> atomically $ writeTChan worldInputChan ReleaseLeft
        GLFW.Key'D      -> atomically $ writeTChan worldInputChan ReleaseRight
        GLFW.Key'W      -> atomically $ writeTChan worldInputChan ReleaseUp
        GLFW.Key'S      -> atomically $ writeTChan worldInputChan ReleaseDown
        GLFW.Key'Space  -> atomically $ writeTChan worldInputChan ReleaseSpace
        _               -> return ()
    _                     -> return ()

processWorldInput :: ClientState -> IO ClientState
processWorldInput clientState@ClientState{..} = join $ atomically $ do
  emptyChan <- isEmptyTChan worldInputChan
  if not emptyChan
    then do
        message <- readTChan worldInputChan
        return $ do
          processWorldInput $ processInputMessage clientState message
    else do
      return $ pure clientState

processInputMessage :: ClientState -> WorldInputMessage -> ClientState
processInputMessage clientState@ClientState{..} message = case message of
  PressUp      -> clientState { keysState = keysState {up = True} }
  PressLeft    -> clientState { keysState = keysState {left = True} }
  PressDown    -> clientState { keysState = keysState {down = True} }
  PressRight   -> clientState { keysState = keysState {right = True} }
  PressSpace   -> clientState { keysState = keysState {space = True} }
  ReleaseUp    -> clientState { keysState = keysState {up = False} }
  ReleaseLeft  -> clientState { keysState = keysState {left = False} }
  ReleaseDown  -> clientState { keysState = keysState {down = False} }
  ReleaseRight -> clientState { keysState = keysState {right = False} }
  ReleaseSpace -> clientState { keysState = keysState {space = False} }