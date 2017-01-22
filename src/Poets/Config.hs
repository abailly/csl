{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Config
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Patrick Bahr
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module provides functions to obtain configuration values for POETS.
--
--------------------------------------------------------------------------------

module Poets.Config
    (
     PoetsConfig(..),
     EventsConfig(..),
     ServerConfig(..),
     RuleConfig(..),
     parseConfigFile
    ) where

import Data.ConfigFile
import Control.Monad.Error
import System.Log.Logger
import Network.Socket.Internal

data PoetsConfig = PoetsConfig {
      rulesConfig :: RuleConfig,
      eventsConfig :: EventsConfig,
      serverConfig :: ServerConfig
    }

data RuleConfig = RuleConfig {
} 

data ServerConfig = ServerConfig {
      serverPort :: PortNumber,
      loggingFile :: FilePath,
      loggingPriority :: Priority
    }

data EventsConfig = EventsConfig {
      eventsLogfile :: FilePath
    }

parseConfigFile :: FilePath -> IO (Either String PoetsConfig)
parseConfigFile configFile = do
  res <- runErrorT $ do
     cfg <- join $ liftIO $ readfile emptyCP configFile
     rulesConf <- readRulesConfig cfg
     eventsConf <-  readEventsConfig cfg
     serverConf <-  readServerConfig cfg
     return PoetsConfig { 
                  rulesConfig = rulesConf,
                  serverConfig = serverConf,
                  eventsConfig = eventsConf}
  return $ either (Left . snd) Right res

readRulesConfig :: MonadError CPError m => ConfigParser -> m RuleConfig
readRulesConfig _ =
  return RuleConfig { }

readServerConfig :: MonadError CPError m => ConfigParser -> m ServerConfig
readServerConfig cfg = do
  let section = "server"
  (port :: Integer) <- get cfg section "port"
  loggingFile <- get cfg section "logfile"
  loggingPriority <- get cfg section "logpriority"
  return ServerConfig {serverPort = fromInteger port,
                       loggingFile = loggingFile,
                       loggingPriority = read loggingPriority}

readEventsConfig :: MonadError CPError m => ConfigParser -> m EventsConfig
readEventsConfig cfg = do
  let section = "events"
  logfile <- get cfg section "logfile"
  return EventsConfig {eventsLogfile = logfile}
