{-# LANGUAGE MultiParamTypeClasses #-}

module GMessages.Network.Converter (
  convert,
  convertWithKey
) where

import qualified GMessages.Network.ClientServer as CS
import qualified GMessages.Network.ServerClient as SC
import qualified GMessages.Client as C
import qualified GMessages.Server as S
import GCommon.Types.Generic (ClientKey)



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
  convertWithKey :: a -> ClientKey -> S.KeyMessage b

instance ConverterWithKey CS.WorldMessage S.WorldMessage where
  convertWithKey message key = S.KeyMessage key $ case message of
    CS.PositionUpdate position -> S.PositionUpdate position
    CS.Fire                    -> S.Fire

instance ConverterWithKey CS.PingMessage S.PingMessage where
  convertWithKey message key = S.KeyMessage key $ case message of
    CS.PingRequest -> S.PingRequest

instance ConverterWithKey CS.ConnectionMessage S.ConnectionMessage where
  convertWithKey message key = S.KeyMessage key $ case message of
    CS.ConnectionRequest settings -> S.ConnectionRequest settings
    CS.ConnectionTerminated       -> S.ConnectionTerminated