{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, FlexibleInstances,
  FlexibleContexts, DoAndIfThenElse #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.EventLog
-- Copyright   :  3gERP, 2012
-- License     :  All Rights Reserved
-- 
-- Maintainer  :  Patrick Bahr, Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- Event log for the POETS system.
-- 
--------------------------------------------------------------------------------

module Poets.EventLog
    (
     Event,
     EventLog,
     EventLogCallback(..),
     LogError(..),
     logAddDataDefsEvent,
     logCreateReportEvent,
     logUpdateReportEvent,
     logDeleteReportEvent,
     logCreateEntityEvent,
     logUpdateEntityEvent,
     logDeleteEntityEvent,
     logCreateContractDefEvent,
     logUpdateContractDefEvent,
     logDeleteContractDefEvent,
     logCreateContractEvent,
     logUpdateContractEvent,
     logDeleteContractEvent,
     logConcludeContractEvent,
     logTransactionEvent,
     transactionEvent,
     getEvents,
     getEntityStore,
     getDataEngine,
     getEntityTypingEnv,
     createLog,
     bootstrapLog,
     stopLog
    ) where

import Data.IORef
import Poets.Conc
import Poets.Config
import Poets.Logging
import Poets.EntityStore as ES hiding (TypeError)
import Poets.EventLog.Names as Names
import Poets.Data hiding (RunTimeError)
import Control.Monad
import Control.Monad.Error
import Control.Concurrent
import System.FilePath
import System.Directory
import System.IO
import Text.XML.Light


--------------------------------------------------------------------------------
-- Log specific event types
--------------------------------------------------------------------------------

-- |Add a @CreateReport@ event to the event log.
logCreateReportEvent :: EventLog
                     -> String -- ^Report name.
                     -> String -- ^Report description.
                     -> Value -- ^Report tags.
                     -> String -- ^Report code.
                     -> IO (Either LogError ())
logCreateReportEvent eventLog name desc tags code =
    logEvent eventLog $ createReport name desc code tags

-- |Add an @UpdateReport@ event to the event log.
logUpdateReportEvent :: EventLog
                     -> String -- ^Report name.
                     -> String -- ^Report description.
                     -> Value -- ^Report tags.
                     -> String -- ^Report code.
                     -> IO (Either LogError ())
logUpdateReportEvent eventLog name desc tags code =
    logEvent eventLog $ updateReport name desc code tags

-- |Add a @DeleteReport@ event to the event log.
logDeleteReportEvent :: EventLog
                     -> String -- ^Report name.
                     -> IO (Either LogError ())
logDeleteReportEvent eventLog name =
    logEvent eventLog $ deleteReport name

-- |Add a @CreateContract@ event to the event log.
logCreateContractEvent :: EventLog
                       -> Int -- ^Contract ID.
                       -> Value -- ^Contract meta data.
                       -> IO (Either LogError ())
logCreateContractEvent eventLog cId m =
    logEvent eventLog $ createContract cId m

-- |Add an @UpdateContract@ event to the event log.
logUpdateContractEvent :: EventLog
                       -> Int -- ^Contract ID.
                       -> Value -- ^Contract meta data.
                       -> IO (Either LogError ())
logUpdateContractEvent eventLog cId m =
    logEvent eventLog $ updateContract cId m

-- |Add a @DeleteContract@ event to the event log.
logDeleteContractEvent :: EventLog
                       -> Int -- ^Contract ID.
                       -> IO (Either LogError ())
logDeleteContractEvent eventLog cId =
    logEvent eventLog $ deleteContract cId

-- |Add a @ConcludeContract@ event to the event log.
logConcludeContractEvent :: EventLog
                         -> Int -- ^Contract ID.
                         -> IO (Either LogError ())
logConcludeContractEvent eventLog cId =
    logEvent eventLog $ concludeContract cId

-- |Add a @TransactionEvent@ event to the event log.
logTransactionEvent :: EventLog
                    -> Int -- ^Contract ID.
                    -> DateTime -- ^Posting date.
                    -> Value -- ^Transaction data.
                    -> IO (Either LogError ())
logTransactionEvent eventLog cId dt tr =
    logEvent eventLog $ transactionEvent cId dt tr

-- |Add a @CreateContractDef@ event to the event log.
logCreateContractDefEvent :: EventLog
                          -> String -- ^Name.
                          -> RecordName -- ^Type.
                          -> String -- ^Description.
                          -> String -- ^CSL code.
                          -> IO (Either LogError ())
logCreateContractDefEvent eventLog name tp desc code =
    logEvent eventLog $ createContractDef name tp desc code             

-- |Add an @UpdateContractDef@ event to the event log.
logUpdateContractDefEvent :: EventLog
                          -> String -- ^Name.
                          -> RecordName -- ^Type.
                          -> String -- ^Description.
                          -> String -- ^CSL code.
                          -> IO (Either LogError ())
logUpdateContractDefEvent eventLog name tp desc code =
    logEvent eventLog $ updateContractDef name tp desc code

-- |Add a @DeleteContractDef@ event to the event log.
logDeleteContractDefEvent :: EventLog
                          -> String -- ^Name.
                          -> IO (Either LogError ())
logDeleteContractDefEvent eventLog name =
    logEvent eventLog $ deleteContractDef name

{-|
  This function logs an event in the event log. It adds the current time as the
  (internal) time stamp of the event, which is also returned. The event is
  type checked before it is persisted to the log, which amounts to also checking
  for referential integrity.
-}
logEvent :: EventLog -> Event -> IO (Either LogError ())
logEvent eventLog@EventLog{logLock = lock,
                           logDataEngine = dataEngine,
                           logEntityStore = storeR} event = 
    atomic lock $ withRecordEnv dataEngine $ \recordEnv -> do
      dt <- getTime eventLog
      store <- readIORef storeR
      case addField' eventFieldInternalTimeStamp (iVDateTime dt) event of
        Left _ ->
            return $ throwError $ RunTimeError
                     "Failed to add internal time stamp to event"
        Right event' ->
            case typeCheckEvent recordEnv store False event' of
              Left err ->
                  return $ throwError $ TypeError err
              Right () -> do
                  writeEvent eventLog event'
                  return $ return ()

-- |Retrieve the (internal) timstamp to assign to an event.
getTime :: EventLog -> IO DateTime
getTime EventLog{logCurrentTime = ct} = maybe getCurrentDateTime return ct

-- |Write an event to the event log file (only if the log is active). This
-- function assumes that a lock has already been acquired.
writeEvent :: EventLog -> Event -> IO ()
writeEvent eventLog event = do
  active <- readIORef $ logActive eventLog
  when active $ do modifyIORef (logEvents eventLog) (event :)
                   file <- readIORef $ logFile eventLog
                   hPutStrLn file (serializeToString event)

-- |Add data definitions to the system, and persist an @AddDataDefs@ event to
-- the event log.
logAddDataDefsEvent :: EventLog -> [String] -> IO (Either LogError ())
logAddDataDefsEvent eventLog pces =
    atomic lock $ modifyRecordEnv dataEngine $ \recordEnv upd -> do
      dt <- getTime eventLog
      store <- readIORef storeR
      let event = Names.addDataDefs dt pces
      events <- getEvents eventLog
      -- First check that the new data definitions are valid
      case Poets.Data.addDataDefs recordEnv (map (\x -> (x,"<noname>")) pces) of
        Left err ->
            return $ throwError $ DataDefError err
        Right recordEnv' ->
            -- Then type check all existing events with respect to the new
            -- record environment
            case typeCheckEvents recordEnv' store events of
              Left err ->
                  return $ throwError $ TypeError err
              Right () -> do
                -- TODO: type check existing contracts and reports
                upd recordEnv'
                writeEvent eventLog event
                return $ return ()
        where dataEngine = logDataEngine eventLog
              storeR = logEntityStore eventLog
              lock = logLock eventLog
              typeCheckEvents :: POETSRecordEnv
                              -> EntityStore
                              -> [Event] -> Either String ()
              typeCheckEvents recordEnv store =
                  -- Important: type check in accumulated entity typing env.
                  mapM_ (typeCheckEvent recordEnv store True)

-- |Create a new entity, and persist a @CreateEntity@ event to the event log.
-- The generated ID is returned.
logCreateEntityEvent :: EventLog
                     -> Value -- ^Entity data.
                     -> RecordName -- ^Entity type.
                     -> IO (Either LogError EntId)
logCreateEntityEvent eventLog@EventLog{logLock = lock,
                                       logDataEngine = dataEngine,
                                       logEntityStore = storeR} v rName =
    atomic lock $ withRecordEnv dataEngine $ \recordEnv -> do
      dt <- getTime eventLog
      store <- readIORef storeR
      case extractRecord v of
        Left err ->
            return $ throwError $ RunTimeError err
        Right r ->
            case ES.createEntity store recordEnv r rName dt of
              Left err ->
                  return $ throwError $ EntityStoreError err
              Right (newId, newStore) ->
                  let event = Names.createEntity dt r rName newId in
                  case typeCheckEvent recordEnv newStore False event of
                    Left err ->
                        return $ throwError $ TypeError err
                    Right () -> do
                        writeIORef storeR newStore
                        writeEvent eventLog event
                        return $ return newId

-- |Update an existing entity, and persist a @UpdateEntity@ event to the event
-- log.
logUpdateEntityEvent :: EventLog
                     -> Int -- ^Entity ID.
                     -> Value -- ^Entity data.
                     -> IO (Either LogError ())
logUpdateEntityEvent eventLog@EventLog{logLock = lock,
                                       logDataEngine = dataEngine,
                                       logEntityStore = storeR} id v =
    atomic lock $ withRecordEnv dataEngine $ \recordEnv -> do
      dt <- getTime eventLog
      store <- readIORef storeR
      case extractRecord v of
        Left err ->
            return $ throwError $ RunTimeError err
        Right r ->
            case ES.updateEntity store recordEnv id r dt of
              Left err ->
                  return $ throwError $ EntityStoreError err
              Right newStore ->
                  let event = Names.updateEntity dt r id in
                  case typeCheckEvent recordEnv newStore False event of
                    Left err ->
                        return $ throwError $ TypeError err
                    Right () -> do
                        writeIORef storeR newStore
                        writeEvent eventLog event
                        return $ return ()

-- |Delete an existing entity, and persist a @DeleteEntity@ event to the event
-- log.
logDeleteEntityEvent :: EventLog
                     -> EntId -- ^Entity ID.
                     -> IO (Either LogError ())
logDeleteEntityEvent eventLog@EventLog{logLock = lock,
                                       logDataEngine = dataEngine,
                                       logEntityStore = storeR} id =
    atomic lock $ withRecordEnv dataEngine $ \recordEnv -> do
      dt <- getTime eventLog
      store <- readIORef storeR
      case ES.deleteEntity store id of
        Left err ->
            return $ throwError $ EntityStoreError err
        Right (rName, newStore) ->
            let event = Names.deleteEntity dt rName id in
            case typeCheckEvent recordEnv store False event of
              Left err ->
                  return $ throwError $ TypeError err
              Right () -> do
                  writeIORef storeR newStore
                  writeEvent eventLog event
                  return $ return ()


--------------------------------------------------------------------------------
-- Event log implementation
--------------------------------------------------------------------------------

{-|
  Abstract data type representing a running event log.
-}
data EventLog = EventLog
    { logEvents :: IORef [Event],
      logLock :: Lock,
      logFile :: IORef Handle,
      logConfig :: EventsConfig,
      logActive :: IORef Bool,
      logFlushThread :: IORef ThreadId,
      logDataEngine :: DataEngine,
      logEntityStore :: IORef EntityStore,
      logCurrentTime :: Maybe DateTime}

getDataEngine :: EventLog -> DataEngine
getDataEngine = logDataEngine

-- |Call back functions for bootstrapping the system. These functions will be
-- invoked when the appropriate events are processed during bootstrapping of the
-- event log.
data EventLogCallback = EventLogCallback{
      -- |Create a report.
      cbCreateReport :: String -> String -> [String] -> String -> IO (),
      -- |Update a report.
      cbUpdateReport :: String -> String -> [String] -> String -> IO (),
      -- |Delete a report.
      cbDeleteReport :: String -> IO (),
      -- |Create a contract definition.
      cbCreateContractDef :: String -> IO (),
      -- |Update a contract definition.
      cbUpdateContractDef :: String -> IO (),
      -- |Delete a contract definition.
      cbDeleteContractDef :: String -> IO (),
      -- |Create a contract.
      cbCreateContract :: Value -> IO Int,
      -- |Update a contract.
      cbUpdateContract :: Int -> Value -> IO (),
      -- |Delete a contract.
      cbDeleteContract :: Int -> IO (),
      -- |Conclude a contract.
      cbConcludeContract :: Int -> IO (),
      -- |Register a transaction against a contract.
      cbTransaction :: Int -> DateTime -> Value -> IO ()
    }

-- |Errors that may occur during run-time.
data LogError =
    -- |Unexpected runtime error (i.e., \"this should not happen\").
    RunTimeError String
    -- |Entity store related error
  | EntityStoreError EntityStoreError
    -- |Type error.
  | TypeError String
    -- |Data definition Error
  | DataDefError DataError

instance Show LogError where
    show (RunTimeError e) =
        "Runtime error: " ++ e
    show (EntityStoreError e) =
        show e
    show (TypeError e) =
        "Type error: " ++ e
    show (DataDefError e) =
        "Data definition error: " ++ show e

instance Error LogError where
    strMsg = RunTimeError

-- |Type check a POETS event w.r.t. the supplied record typing environment.
typeCheckEvent :: POETSRecordEnv -- ^The record typing environment.
               -> EntityStore -- ^The entity store.
               -> Bool -- ^Indication whether to check in accumulated entity typing environment.
               -> Event -- ^The event to type check.
               -> Either String ()
typeCheckEvent recordEnv store acc =
    typeCheckRecord recordEnv (getTypingEnv acc store) eventClass
        
{-|
  This function returns a lazy list containing a the current events of 
  the given event log.
-}
getEvents :: EventLog -> IO [Event]
getEvents EventLog{logEvents = events} = readIORef events

-- |Retrieve the current entity store.
getEntityStore :: EventLog -> IO EntityStore
getEntityStore EventLog{logEntityStore = storeR} = readIORef storeR

-- |Retrieve the entity typing environment of the current store.
getEntityTypingEnv :: EventLog -> IO EntityTypingEnv
getEntityTypingEnv EventLog{logEntityStore = storeR} =
    liftM (getTypingEnv False) $ readIORef storeR

flushEventLogBuffer fileR lock activeR = flushLoop
    where flushLoop = do
            threadDelay 10000000
            atomic lock $ do
              active <- readIORef activeR
              when active (readIORef fileR >>= hFlush)
            flushLoop

-- |Stop the event log.
stopLog :: EventLog -> IO ()
stopLog EventLog{ logFile = fileR, logActive = activeR, logLock = lock} = 
    atomic_ lock $ do
      active <- readIORef activeR
      when active $ do
        writeIORef activeR False
        file <- readIORef fileR
        hClose file

{-|
  This function creates a new event log object. Events are /not/ processed from
  the file (use 'bootstrapLog' for this purpose).
-}
createLog :: EventsConfig -> DataEngine -> IO EventLog
createLog cfg dataEngine = do
  lock <- newLock
  eventsR <- newIORef []
  storeR <- newIORef newEntityStore
  fileR <- newIORef stdout
  activeR <- newIORef False
  flush <- forkIO $ flushEventLogBuffer fileR lock activeR
  flushR <- newIORef flush
  return EventLog{logEvents = eventsR,
                  logLock = lock,
                  logConfig = cfg,
                  logFile = fileR,
                  logActive = activeR,
                  logFlushThread = flushR,
                  logDataEngine = dataEngine,
                  logEntityStore = storeR,
                  logCurrentTime = Nothing}

{-|
  This function bootstraps an event log from a file. Each event is processed
  sequentially, and the relevant call back functions are invoked. The log is
  activated after all events have been processed.
-}
bootstrapLog :: EventLog -> EventLogCallback -> IO ()
bootstrapLog eventLog callbacks = do
  infoEL "Starting event log"
  curDir <- getCurrentDirectory
  debugEL $ "Bootstrapping system from " ++ show (curDir </> filePath)
  fileExists <- doesFileExist filePath
  unless fileExists (do let dir = takeDirectory filePath
                        createDirectoryIfMissing True dir
                        writeFile filePath "")
  contents <- readFile filePath
  let xmlElems = onlyElems $ parseXML contents
  -- Process each event sequentially
  mapM_ (\elem -> do e <- parse elem
                     let Just dt = extractInternalTimeStamp e
                     processEvent eventLog{logCurrentTime = Just dt} e
                     modifyIORef eventsR (e :)) xmlElems
  file <- openFile filePath AppendMode
  writeIORef fileR file
  writeIORef activeR True
  infoEL "Event log successfully started"
  return ()
  where fileR = logFile eventLog
        filePath = eventsLogfile $ logConfig eventLog
        activeR = logActive eventLog
        eventsR = logEvents eventLog
        -- Parse an XML element to an event.
        parse :: Element -> IO Event
        parse elem =
            case deSerialize elem of
              Left msg -> do
                emergencyEL $
                     "An error occurred while parsing the element \"" ++
                     showElement elem ++ "\":\n"++ msg
                fail $ "Failed to read event log from \"" ++ filePath ++ "\""
              Right event -> 
                return event
        -- Process a single event by dispatch on the event type
        processEvent :: EventLog -> Event -> IO ()        
        processEvent eventLog event
            | isAddDataDefsEvent event = do
                let Just pce = extractDataDefs event
                res <- logAddDataDefsEvent eventLog pce
                either (fail . show)
                       (\() -> debugEL "Data definitions updated")
                       res
            | isCreateReportEvent event = do
                let Just name = extractReportName event
                let Just desc = extractReportDescription event
                let Just tags = extractReportTags event
                let Just code = extractReportParrotCode event
                cbCreateReport callbacks name desc tags code
                debugCE $ "Report " ++ name ++ " created"
            | isUpdateReportEvent event = do
                let Just name = extractReportName event
                let Just desc = extractReportDescription event
                let Just tags = extractReportTags event
                let Just code = extractReportParrotCode event
                cbUpdateReport callbacks name desc tags code
                debugCE $ "Report " ++ name ++ " updated"
            | isDeleteReportEvent event = do
                let Just name = extractReportName event
                cbDeleteReport callbacks name
                debugCE $ "Report " ++ name ++ " deleted"
            | isCreateEntityEvent event = do
                let Just d = extractEntityData event
                let Just t = extractEntityType event
                let Just id = fmap ventId $ extractEntity event
                res <- logCreateEntityEvent eventLog d t
                either (fail . show)
                       (\id' -> do
                          when (id /= id')
                               (do emergencyCE $ "Inconsistency when creating "
                                                 ++ "entity with ID " ++
                                                 show id ++ ", got new ID " ++
                                                 show id'
                                   fail $ "Failed to read event log from \"" ++
                                          filePath ++ "\"")
                          debugEL $ "Entity " ++ show id ++ " created")
                       res
            | isUpdateEntityEvent event = do
                let Just d = extractEntityData event
                let Just id = fmap ventId $ extractEntity event
                res <- logUpdateEntityEvent eventLog id d
                either (fail . show)
                       (\() -> debugEL $ "Entity " ++ show id ++ " updated")
                       res
            | isDeleteEntityEvent event = do
                let Just id = fmap ventId $ extractEntity event
                res <- logDeleteEntityEvent eventLog id
                either (fail . show)
                       (\() -> debugEL $ "Entity " ++ show id ++ " deleted")
                       res
            | isCreateContractDefEvent event = do
                let Just name = extractContractDefName event
                let Just code = extractContractDefCSLCode event
                cbCreateContractDef callbacks code
                debugCE $ "Contract definition " ++ name ++ " created"
            | isUpdateContractDefEvent event = do
                let Just name = extractContractDefName event
                let Just code = extractContractDefCSLCode event
                cbUpdateContractDef callbacks code
                debugCE $ "Contract definition " ++ name ++ " updated"
            | isDeleteContractDefEvent event = do
                let Just name = extractContractDefName event
                cbDeleteContractDef callbacks name
                debugCE $ "Contract definition " ++ name ++ " deleted"
            | isCreateContractEvent event = do
                let Just cId = extractContractId event
                let Just m = extractContractMetaData event
                cId' <- cbCreateContract callbacks m
                when (cId /= cId')
                     (do emergencyCE $ "Inconsistency when creating " ++
                                       " contract with ID " ++
                                       show cId ++ ", got new ID " ++ show cId'
                         fail $ "Failed to read event log from \"" ++
                                filePath ++ "\"")
                debugEL $ "Contract " ++ show cId ++ " created"
            | isUpdateContractEvent event = do
                let Just cId = extractContractId event
                let Just m = extractContractMetaData event
                cbUpdateContract callbacks cId m
                debugEL $ "Contract " ++ show cId ++ " updated"
            | isDeleteContractEvent event = do
                let Just cId = extractContractId event
                cbDeleteContract callbacks cId
                debugEL $ "Contract " ++ show cId ++ " deleted"
            | isConcludeContractEvent event = do
                let Just cId = extractContractId event
                cbConcludeContract callbacks cId
                debugEL $ "Contract " ++ show cId ++ " concluded"
            | isTransactionEvent event = do
                let Just cId = extractContractId event
                let Just dt = extractTimeStamp event
                let Just tr = extractTransaction event
                cbTransaction callbacks cId dt tr
                return ()
            | otherwise = do
                emergencyEL $
                     "Unknown event type:\n" ++ serializeToString event
                fail $ "Failed to read event log from \"" ++ filePath ++ "\""