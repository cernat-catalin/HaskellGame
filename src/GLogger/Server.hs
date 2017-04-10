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
import System.Log.Formatter (simpleLogFormatter)
import System.IO (withFile, IOMode(..))


logFileName :: String
logFileName = "server.log"

loggerName :: String
loggerName = "serverLogger"

initLogger :: IO ()
initLogger = do
  h <- fileHandler logFileName SL.DEBUG >>= \lh -> return $
                  setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  SL.updateGlobalLogger loggerName (SL.setLevel SL.DEBUG)
  SL.updateGlobalLogger loggerName (SL.addHandler h)

cleanLog :: IO ()
cleanLog = withFile logFileName WriteMode (\_ -> return ())

logDebug :: String -> IO ()
logDebug = SL.debugM loggerName

logInfo :: String -> IO ()
logInfo = SL.infoM loggerName

logNotice :: String -> IO ()
logNotice = SL.noticeM loggerName

logWarning :: String -> IO ()
logWarning = SL.warningM loggerName

logError :: String -> IO ()
logError = SL.errorM loggerName

logCritical :: String -> IO ()
logCritical = SL.criticalM loggerName

logAlert :: String -> IO ()
logAlert = SL.alertM loggerName

logEmergency :: String -> IO ()
logEmergency = SL.emergencyM loggerName