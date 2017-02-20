{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Data
-- Copyright   :  3gERP, 2012
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved, Patrick Bahr, and Morten Ib Nielsen
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module is a wrapper for the data engine. The data engine maintains the
-- data definitions (i.e., record definitions) in-memory.
--
--------------------------------------------------------------------------------

module Poets.Data
    (
     DataEngine,
     DataError(..),
     createDataEngine,
     withRecordEnv,
     modifyRecordEnv,
     addDataDefs,
     typeCheckRecord,
     typeCheckValue,
     module X
    ) where

import           Data.Comp                    as X
import           Data.IORef
import           Data.List                    (find)
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import           Poets.Conc
import           Poets.Data.Render
import           Poets.Data.Serialize         as X
import           Poets.Data.Type              as X
import           Poets.Data.Type.Parser
import           Poets.Data.Type.Utils        as X
import           Poets.Data.Value             as X
import           Poets.Data.Value.Serialize   ()
import           Poets.Data.Value.TypeChecker (typeCheckTerm)
import           Poets.Data.Value.Utils       as X
import           Poets.Logging

-- |A handle to the data engine.
data DataEngine = DataEngine
    {
      -- |Data definitions can be accessed by multiple readers, but only one
      -- writer. Furthermore, writers exclude simultaneous readers.
      dataEngineLock       :: RWLock,
      -- |The in-memory data/record definitions.
      dataEngineRecordEnvR :: IORef POETSRecordEnv
    }

data DataError = ValidationError String
               | ParseError ParseError
               | RunTimeError String

instance Show DataError where
    show (ValidationError e) = "Validation error: " ++ e
    show (ParseError e)      = "Parse error: " ++ show e
    show (RunTimeError e)    = "Runtime error: " ++ e

-- |Create a handle to the data engine. The parameter is the initial ontology of
-- the system, which will be protected from future changes.
createDataEngine :: [(String,FilePath)] -> IO DataEngine
createDataEngine defs = do
  infoData "Starting data engine"
  -- Parse each file, and construct the initial ontology (left to right)
  recordEnv <- either (fail . show) return $
               foldl (\mr (pce,path) -> mr >>= parsePCE path pce)
                     (return $ newRecordEnv []) defs
  either fail return $ validateRecordEnv recordEnv
  -- Set all initial record definitions to be locked for later changes
  let recordEnv' = Map.map (`addRecordAttribute` RecordIsLocked)
                           (recordEnvMap recordEnv)
  debugData "Data engine successfully started"
  dataEngineLock <- newRWLock
  recordEnvR <- newIORef $ newRecordEnvMap recordEnv'
  return DataEngine{dataEngineLock = dataEngineLock,
                    dataEngineRecordEnvR = recordEnvR}

-- |Read the current record/data definitions.
withRecordEnv :: DataEngine -> (POETSRecordEnv -> IO a) -> IO a
withRecordEnv DataEngine{dataEngineLock = lock,
                         dataEngineRecordEnvR = recordEnvR} m =
  rwReader lock $ readIORef recordEnvR >>= m

-- |Modify the current record/data definitions. The monadic computation gets a
-- copy of the current data definitions, as well as a handle for writing new
-- definitions.
modifyRecordEnv :: DataEngine
                -> (POETSRecordEnv -> (POETSRecordEnv -> IO ()) -> IO a) -> IO a
modifyRecordEnv DataEngine{dataEngineLock = lock,
                           dataEngineRecordEnvR = recordEnvR} m =
  rwWriter lock $ do recordEnv <- readIORef recordEnvR
                     m recordEnv (writeIORef recordEnvR)

-- |Add a set of data definitions to an existing record typing environment. The
-- function checks that no locked record types change, that no new root types
-- are introduced, and finally that the new record typing environment is valid.
addDataDefs :: POETSRecordEnv
            -> [(String,FilePath)]
            -> Either DataError POETSRecordEnv
addDataDefs recordEnv files = do
  -- Parse each file, and construct the ontology (left to right)
  recordEnv' <- either (Left . ParseError) return
                       (foldl (\mr (pce,path) -> mr >>= parsePCE path pce)
                              (return recordEnv) files)
  -- Then check that no locked record definitions have been changed
  let locked = filter isLocked (recordEnvList recordEnv)
  let validationError = Left . ValidationError
  case find (`notElem` recordEnvList recordEnv') locked of
    Just r ->
        validationError $ "Locked record type '" ++ recordName r ++
                          "' cannot be changed"
    _ ->
        -- Then check that no new root elements have been added
        let newRoots = filter (Set.null . recordExtends)
                              (recordEnvList recordEnv') in
        case find (`notElem` recordEnvList recordEnv) newRoots of
          Just r ->
              validationError $ "record type '" ++ recordName r ++
                                "' must have a super type"
          _ ->
              -- Then check if the new record environment is valid
              case validateRecordEnv recordEnv' of
                Left err -> validationError err
                Right () -> return recordEnv'

-- |Type check a POETS value, and check that is a sub type of the specified
-- record type.
typeCheckRecord :: (TypeConstant :<: t, TypeList :<: t, TypeEnt :<: t,
                    EqF t, Functor t, Render t) =>
                   RecordEnv (Term t) -- ^The record typing environment.
                -> EntityTypingEnv -- ^The entity typing environment.
                -> RecordName -- ^The record (super) type.
                -> Value -- ^The value to type check.
                -> Either String ()
typeCheckRecord recordEnv entityEnv rName =
    typeCheckValue recordEnv entityEnv (iTRecord rName)

-- |Type check a POETS value, and check that is of the given type
typeCheckValue :: (TypeConstant :<: t, TypeList :<: t, TypeEnt :<: t,
                   EqF t, Functor t, Render t) =>
                  RecordEnv (Term t) -- ^The record typing environment.
               -> EntityTypingEnv -- ^The entity typing environment.
               -> Term t -- ^The expected type
               -> Value -- ^The value to type check.
               -> Either String ()
typeCheckValue recordEnv entityEnv ty = typeCheckTerm ty recordEnv entityEnv
