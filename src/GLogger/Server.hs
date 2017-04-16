module GLogger.Server (
  initLogger,
  cleanLog,
  logDebug,
  logInfo,
  logNotice,
  logWarning,
  logError,
  logCritical,
  logAlert,
  logEmergency
  ) where

import qualified System.Log.Logger as SL
import System.Log.Handler.Simple (fileHandler)
import System.Log.Handler (setFormatter)
import System.Log.Formatter (tfLogFormatter)
import System.IO (withFile, IOMode(..))
import Control.Monad (when)


logFileName :: String
logFileName = "server.log"

loggerName :: String
loggerName = "serverLogger"

enableLogging :: Bool
enableLogging = True

initLogger :: IO ()
initLogger = when enableLogging $ do
  h <- fileHandler logFileName SL.DEBUG >>= \lh -> return $
                  setFormatter lh (tfLogFormatter "%T:%q" "[$time: $loggername : $prio] $msg")
  SL.updateGlobalLogger loggerName (SL.setLevel SL.DEBUG)
  SL.updateGlobalLogger loggerName (SL.addHandler h)

cleanLog :: IO ()
cleanLog = when enableLogging $ do
  withFile logFileName WriteMode (\_ -> return ())

logDebug :: String -> IO ()
logDebug string = when enableLogging $ do
  SL.debugM loggerName string

logInfo :: String -> IO ()
logInfo string = when enableLogging $ do
  SL.infoM loggerName string

logNotice :: String -> IO ()
logNotice string = when enableLogging $ do
  SL.noticeM loggerName string

logWarning :: String -> IO ()
logWarning string = when enableLogging $ do
  SL.warningM loggerName string

logError :: String -> IO ()
logError string = when enableLogging $ do
  SL.errorM loggerName string

logCritical :: String -> IO ()
logCritical string = when enableLogging $ do
  SL.criticalM loggerName string

logAlert :: String -> IO ()
logAlert string = when enableLogging $ do
  SL.alertM loggerName string

logEmergency :: String -> IO ()
logEmergency string = when enableLogging $ do
  SL.emergencyM loggerName string