--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Logging
-- Copyright   :  3gERP, 2009
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module implements logging for the POETS server.
--
--------------------------------------------------------------------------------

module Poets.Logging where

import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler hiding (setLevel)
import System.Log.Handler.Simple
import System.FilePath
import System.Directory
import Poets.Config
import Control.Monad

-- Define the different kinds of modules in POETS
modules = [modulePServer,
           moduleData,
           moduleEventLog,
           moduleContractEngine,
           moduleReporting,
           moduleRuleEngine]
modulePServer = "PServer"
moduleData = "Data"
moduleEventLog = "EventLog"
moduleContractEngine = "ContractEngine"
moduleReporting = "Reporting"
moduleRuleEngine = "RuleEngine"

-- Module-specific logging
debugPServer = debugM modulePServer
infoPServer = infoM modulePServer
noticePServer = noticeM modulePServer
warningPServer = warningM modulePServer
errorPServer = errorM modulePServer
criticalPServer = criticalM modulePServer
alertPServer = alertM modulePServer
emergencyPServer = emergencyM modulePServer

debugData = debugM moduleData
infoData = infoM moduleData
noticeData = noticeM moduleData
warningData = warningM moduleData
errorData = errorM moduleData
criticalData = criticalM moduleData
alertData = alertM moduleData
emergencyData = emergencyM moduleData

debugEL = debugM moduleEventLog
infoEL = infoM moduleEventLog
noticeEL = noticeM moduleEventLog
warningEL = warningM moduleEventLog
errorEL = errorM moduleEventLog
criticalEL = criticalM moduleEventLog
alertEL = alertM moduleEventLog
emergencyEL = emergencyM moduleEventLog

debugCE = debugM moduleContractEngine
infoCE = infoM moduleContractEngine
noticeCE = noticeM moduleContractEngine
warningCE = warningM moduleContractEngine
errorCE = errorM moduleContractEngine
criticalCE = criticalM moduleContractEngine
alertCE = alertM moduleContractEngine
emergencyCE = emergencyM moduleContractEngine

debugReporting = debugM moduleReporting
infoReporting = infoM moduleReporting
noticeReporting = noticeM moduleReporting
warningReporting = warningM moduleReporting
errorReporting = errorM moduleReporting
criticalReporting = criticalM moduleReporting
alertReporting = alertM moduleReporting
emergencyReporting = emergencyM moduleReporting

debugRU = debugM moduleRuleEngine
infoRU = infoM moduleRuleEngine
noticeRU = noticeM moduleRuleEngine
warningRU = warningM moduleRuleEngine
errorRU = errorM moduleRuleEngine
criticalRU = criticalM moduleRuleEngine
alertRU = alertM moduleRuleEngine
emergencyRU = emergencyM moduleRuleEngine

-- |Setup logging based on configuration.
setupLogging :: ServerConfig -> IO ()
setupLogging conf = do
  fileExists <- doesFileExist filePath
  unless fileExists (do let dir = takeDirectory filePath
                        createDirectoryIfMissing True dir
                        writeFile filePath "")
  fileHandler <- fileHandler filePath (loggingPriority conf)
  let fmt = simpleLogFormatter "[$time/$loggername/$prio] $msg"
  updateGlobalLogger rootLoggerName (addHandler $ setFormatter fileHandler fmt)
  mapM_ (\m -> updateGlobalLogger m (setLevel $ loggingPriority conf)) modules
      where filePath = loggingFile conf