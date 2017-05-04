{-# LANGUAGE MultiParamTypeClasses #-}

module GMessages.Network.Converter (
  convert,
  convertWithKey
) where

import qualified GMessages.Network.ClientServer as CS
import qualified GMessages.Network.ServerClient as SC
import qualified GMessages.Client as C
import qualified GMessages.Server as S
import Common.GTypes (ClientKey)



class Converter a b where
  convert :: a -> b

instance Converter SC.WorldMessage C.WorldMessage where
  convert message = case message of
    SC.WorldUpdate world -> C.WorldUpdate world

instance Converter SC.ServiceMessage C.ServiceMessage where
  convert message' = case message' of
    SC.PingMessage message -> C.PingMessage $ case message of
      SC.PingResponse ping -> C.PingResponse ping


class ConverterWithKey a b where
  convertWithKey :: a -> ClientKey -> b

instance ConverterWithKey CS.WorldMessage S.WorldMessage where
  convertWithKey message key = case message of
    CS.PositionUpdate position -> S.PositionUpdate key position

instance ConverterWithKey CS.ServiceMessage S.ServiceMessage where
  convertWithKey message' key = case message' of
    CS.ConnectionMessage message -> S.ConnectionMessage $ case message of
      CS.ConnectionRequest settings -> S.ConnectionRequest key settings
      CS.ConnectionTerminated       -> S.ConnectionTerminated key
    CS.PingMessage message       -> S.PingMessage $ case message of
      CS.PingRequest -> S.PingRequest key