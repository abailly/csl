{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, FlexibleInstances,
  FlexibleContexts #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.EventLog.Names
-- Copyright   :  3gERP, 2011
-- License     :  All Rights Reserved
-- 
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines the names of built-in events and their fields, as well
-- as functions for extracting fields from events.
-- 
--------------------------------------------------------------------------------

module Poets.EventLog.Names
    (
     RecordName,
     FieldName,
     Event,
     -- * Event names
     eventClass,
     -- * Field names
     eventFieldInternalTimeStamp,
     contractEventFieldContractId,
     -- * Creation functions
     addDataDefs,
     createReport,
     updateReport,
     deleteReport,
     createEntity,
     updateEntity,
     deleteEntity,
     createContractDef,
     updateContractDef,
     deleteContractDef,
     createContract,
     updateContract,
     deleteContract,
     concludeContract,
     transactionEvent,
     -- * Extraction functions
     isEventType,
     isAddDataDefsEvent,
     isCreateReportEvent,
     isUpdateReportEvent,
     isDeleteReportEvent,
     isCreateEntityEvent,
     isUpdateEntityEvent,
     isDeleteEntityEvent,
     isCreateContractDefEvent,
     isUpdateContractDefEvent,
     isDeleteContractDefEvent,
     isCreateContractEvent,
     isUpdateContractEvent,
     isDeleteContractEvent,
     isConcludeContractEvent,
     isTransactionEvent,
     extractInternalTimeStamp,
     extractDataDefs,
     extractReportName,
     extractReportDescription,
     extractReportTags,
     extractReportParrotCode,
     extractEntity,
     extractEntityData,
     extractEntityType,
     extractContractDefName,
     extractContractDefCSLCode,
     extractContractId,
     extractContractMetaData,
     extractTimeStamp,
     extractTransaction
    ) where

import Poets.Data hiding (addDataDefs)

-- |An event is a POETS record value, which is a sub class of the 'eventClass'
-- class.
type Event = Value

--------------------------------------------------------------------------------
-- Predefined event names
--------------------------------------------------------------------------------

-- |The super class for events.
eventClass :: RecordName
eventClass = "Event"

-- |The event class for adding ontology fragments.
addDataDefsClass :: RecordName
addDataDefsClass = "AddDataDefs"

-- |The event class for creation of reports.
createReportClass :: RecordName
createReportClass = "CreateReport"

-- |The event class for updating of reports.
updateReportClass :: RecordName
updateReportClass = "UpdateReport"

-- |The event class for deletion of reports.
deleteReportClass :: RecordName
deleteReportClass = "DeleteReport"

-- |The event class for creation of entities.
createEntityClass :: RecordName
createEntityClass = "CreateEntity"

-- |The event class for updating of entities.
updateEntityClass :: RecordName
updateEntityClass = "UpdateEntity"

-- |The event class for deletion of entities.
deleteEntityClass :: RecordName
deleteEntityClass = "DeleteEntity"

-- |The class for creation of contract definitions.
createContractDefClass :: RecordName
createContractDefClass = "CreateContractTemplate"

-- |The class for updating of contract definitions.
updateContractDefClass :: RecordName
updateContractDefClass = "UpdateContractTemplate"

-- |The class for deletion of contract definitions.
deleteContractDefClass :: RecordName
deleteContractDefClass = "DeleteContractTemplate"

-- |The class for creation/instantiation of contracts.
createContractClass :: RecordName
createContractClass = "CreateContract"

-- |The class for updating of contracts.
updateContractClass :: RecordName
updateContractClass = "UpdateContract"

-- |The class for deleting of contracts.
deleteContractClass :: RecordName
deleteContractClass = "DeleteContract"

-- |The class for conclusion of contracts.
concludeContractClass :: RecordName
concludeContractClass = "ConcludeContract"

-- |The super class for transaction events (i.e., events in a contract).
transactionEventClass :: RecordName
transactionEventClass = "TransactionEvent"


--------------------------------------------------------------------------------
-- Predefined field names
--------------------------------------------------------------------------------

-- |The internal timestamp field of an event.
eventFieldInternalTimeStamp :: FieldName
eventFieldInternalTimeStamp = "internalTimeStamp"

-- |The string containing the PCE which is added to the ontology.
addDataDefsFieldDefs :: FieldName
addDataDefsFieldDefs = "defs"

-- |The report name of a create/update/delete report event.
reportFieldName :: FieldName
reportFieldName = "name"

-- |The parrot code of a create/update report event.
putReportFieldParrotCode :: FieldName
putReportFieldParrotCode = "code"

-- |The description of a create/update report event.
putReportFieldDescription :: FieldName
putReportFieldDescription = "description"

-- |The tags of a create/update report event.
putReportFieldTags :: FieldName
putReportFieldTags = "tags"

-- |The entity of a create/update/delete entity event.
entityFieldEntity :: FieldName
entityFieldEntity = "ent"

-- |The data field of a create/update entity event.
putEntityFieldData :: FieldName
putEntityFieldData = "data"

-- |The type field of a create entity event.
createEntityFieldType :: FieldName
createEntityFieldType = "recordType"

-- |The contract definition name of a create/update/delete contract def event.
contractDefFieldName :: FieldName
contractDefFieldName = "name"

-- |The type field of a create/update contract definition event.
putContractDefFieldType :: FieldName
putContractDefFieldType = "recordType"

-- |The description field of a create/update contract definition event.
putContractDefFieldDescription :: FieldName
putContractDefFieldDescription = "description"

-- |The CSL code field of a create/update contract definition event.
putContractDefFieldCSLCode :: FieldName
putContractDefFieldCSLCode = "code"

-- |The ID of the contract to which a contract event belongs.
contractEventFieldContractId :: FieldName
contractEventFieldContractId = "contractId"

-- |The contract meta in a create/update contract event.
putContractFieldMetaData :: FieldName
putContractFieldMetaData = "contract"

-- |The time stamp of a transaction event. (Not to be mistaken with the internal
-- time stamp of events).
transactionEventFieldTimeStamp :: FieldName
transactionEventFieldTimeStamp = "timestamp"

-- |The actual transaction of a transaction event.
transactionEventFieldTransaction :: FieldName
transactionEventFieldTransaction = "transaction"


--------------------------------------------------------------------------------
-- Create events from the components they consist of
--------------------------------------------------------------------------------

addDataDefs :: DateTime -> [String] -> Event
addDataDefs dt pce =
    iVRecord VR{vrecordName = addDataDefsClass,
                vrecordFields = newFields [VF{vfieldName = eventFieldInternalTimeStamp,
                                              vfieldValue = iVDateTime dt},
                                           VF{vfieldName = addDataDefsFieldDefs,
                                              vfieldValue = iVList (map iVString pce)}]}

createReport :: String -> String -> String -> Value -> Event
createReport name desc code tags =
    iVRecord VR{vrecordName = createReportClass,
                vrecordFields =
                    newFields [VF{vfieldName = reportFieldName,
                                  vfieldValue = iVString name},
                               VF{vfieldName = putReportFieldDescription,
                                  vfieldValue = iVString desc},
                               VF{vfieldName = putReportFieldTags,
                                  vfieldValue = tags},
                               VF{vfieldName = putReportFieldParrotCode,
                                  vfieldValue = iVString code}]}

updateReport :: String -> String -> String -> Value -> Event
updateReport name desc code tags =
    iVRecord VR{vrecordName = updateReportClass,
                vrecordFields =
                    newFields [VF{vfieldName = reportFieldName,
                                  vfieldValue = iVString name},
                               VF{vfieldName = putReportFieldDescription,
                                  vfieldValue = iVString desc},
                               VF{vfieldName = putReportFieldTags,
                                  vfieldValue = tags},
                               VF{vfieldName = putReportFieldParrotCode,
                                  vfieldValue = iVString code}]}

deleteReport :: String -> Event
deleteReport name =
    iVRecord VR{vrecordName = deleteReportClass,
                vrecordFields =
                    newFields [VF{vfieldName = reportFieldName,
                                  vfieldValue = iVString name}]}

createEntity :: DateTime -> VRecord Value -> RecordName -> Int -> Event
createEntity dt r t id =
    iVRecord VR{vrecordName = createEntityClass,
                vrecordFields =
                    newFields [VF{vfieldName = eventFieldInternalTimeStamp,
                                  vfieldValue = iVDateTime dt},
                               VF{vfieldName = putEntityFieldData,
                                  vfieldValue = iVRecord r},
                               VF{vfieldName = createEntityFieldType,
                                  vfieldValue = iVString t},
                               VF{vfieldName = entityFieldEntity,
                                  vfieldValue =
                                      iVEnt VEntity{
                                                  ventType = vrecordName r,
                                                  ventId = id,
                                                  ventContext = Just dt}}]}

updateEntity :: DateTime -> VRecord Value -> Int -> Event
updateEntity dt r id =
    iVRecord VR{vrecordName = updateEntityClass,
                vrecordFields =
                    newFields [VF{vfieldName = eventFieldInternalTimeStamp,
                                  vfieldValue = iVDateTime dt},
                               VF{vfieldName = putEntityFieldData,
                                  vfieldValue = iVRecord r},
                               VF{vfieldName = entityFieldEntity,
                                  vfieldValue =
                                      iVEnt VEntity{
                                                  ventType = vrecordName r,
                                                  ventId = id,
                                                  ventContext = Just dt}}]}

deleteEntity :: DateTime -> RecordName -> Int -> Event
deleteEntity dt tp id =
    iVRecord VR{vrecordName = deleteEntityClass,
                vrecordFields =
                    newFields [VF{vfieldName = eventFieldInternalTimeStamp,
                                  vfieldValue = iVDateTime dt},
                               VF{vfieldName = entityFieldEntity,
                                  vfieldValue =
                                      iVEnt VEntity{
                                                  ventType = tp,
                                                  ventId = id,
                                                  ventContext = Just dt}}]}

createContractDef :: String -> String -> String -> String -> Event
createContractDef name tp desc code =
    iVRecord VR{vrecordName = createContractDefClass,
                vrecordFields =
                    newFields [VF{vfieldName = contractDefFieldName,
                                  vfieldValue = iVString name},
                               VF{vfieldName = putContractDefFieldType,
                                  vfieldValue = iVString tp},
                               VF{vfieldName = putContractDefFieldDescription,
                                  vfieldValue = iVString desc},
                               VF{vfieldName = putContractDefFieldCSLCode,
                                  vfieldValue = iVString code}]}

updateContractDef :: String -> String -> String -> String -> Event
updateContractDef name tp desc code =
    iVRecord VR{vrecordName = updateContractDefClass,
                vrecordFields =
                    newFields [VF{vfieldName = contractDefFieldName,
                                  vfieldValue = iVString name},
                               VF{vfieldName = putContractDefFieldType,
                                  vfieldValue = iVString tp},
                               VF{vfieldName = putContractDefFieldDescription,
                                  vfieldValue = iVString desc},
                               VF{vfieldName = putContractDefFieldCSLCode,
                                  vfieldValue = iVString code}]}

deleteContractDef :: String -> Event
deleteContractDef name =
    iVRecord VR{vrecordName = deleteContractDefClass,
                vrecordFields =
                    newFields [VF{vfieldName = contractDefFieldName,
                                  vfieldValue = iVString name}]}

createContract :: Int -> Value -> Event
createContract cId m =
    iVRecord VR{vrecordName = createContractClass,
                vrecordFields =
                    newFields [VF{vfieldName = putContractFieldMetaData,
                                  vfieldValue = m},
                               VF{vfieldName = contractEventFieldContractId,
                                  vfieldValue = iVInt cId}]}

updateContract :: Int -> Value -> Event
updateContract cId m =
    iVRecord VR{vrecordName = updateContractClass,
                vrecordFields =
                    newFields [VF{vfieldName = putContractFieldMetaData,
                                  vfieldValue = m},
                               VF{vfieldName = contractEventFieldContractId,
                                  vfieldValue = iVInt cId}]}

deleteContract :: Int -> Event
deleteContract cId =
    iVRecord VR{vrecordName = deleteContractClass,
                vrecordFields =
                    newFields [VF{vfieldName = contractEventFieldContractId,
                                  vfieldValue = iVInt cId}]}

concludeContract :: Int -> Event
concludeContract cId =
    iVRecord VR{vrecordName = concludeContractClass,
                vrecordFields =
                    newFields [VF{vfieldName = contractEventFieldContractId,
                                  vfieldValue = iVInt cId}]}

transactionEvent :: Int -> DateTime -> Value -> Event
transactionEvent cId dt tr =
    iVRecord VR{vrecordName = transactionEventClass,
                vrecordFields =
                    newFields [VF{vfieldName = contractEventFieldContractId,
                                  vfieldValue = iVInt cId},
                               VF{vfieldName = transactionEventFieldTimeStamp,
                                  vfieldValue = iVDateTime dt},
                               VF{vfieldName = transactionEventFieldTransaction,
                                  vfieldValue = tr}]}



--------------------------------------------------------------------------------
-- Extract values from specific event types
--------------------------------------------------------------------------------

isEventType :: RecordName -> Event -> Bool
isEventType eventType event =
    either (const False) (== eventType) (fmap vrecordName (extractRecord event))

-- |Check if an event is an @AddDataDefs@ event.
isAddDataDefsEvent :: Event -> Bool
isAddDataDefsEvent = isEventType addDataDefsClass

-- |Check if an event is a @CreateReport@ event.
isCreateReportEvent :: Event -> Bool
isCreateReportEvent = isEventType createReportClass

-- |Check if an event is a @UpdateReport@ event.
isUpdateReportEvent :: Event -> Bool
isUpdateReportEvent = isEventType updateReportClass

-- |Check if an event is a @DeleteReport@ event.
isDeleteReportEvent :: Event -> Bool
isDeleteReportEvent = isEventType deleteReportClass

-- |Check if an event is a @CreateEntity@ event.
isCreateEntityEvent :: Event -> Bool
isCreateEntityEvent = isEventType createEntityClass

-- |Check if an event is a @UpdateEntity@ event.
isUpdateEntityEvent :: Event -> Bool
isUpdateEntityEvent = isEventType updateEntityClass

-- |Check if an event is a @DeleteEntity@ event.
isDeleteEntityEvent :: Event -> Bool
isDeleteEntityEvent = isEventType deleteEntityClass

-- |Check if an event is a @CreateContractDef@ event.
isCreateContractDefEvent :: Event -> Bool
isCreateContractDefEvent = isEventType createContractDefClass

-- |Check if an event is a @UpdateContractDef@ event.
isUpdateContractDefEvent :: Event -> Bool
isUpdateContractDefEvent = isEventType updateContractDefClass

-- |Check if an event is a @DeleteContractDef@ event.
isDeleteContractDefEvent :: Event -> Bool
isDeleteContractDefEvent = isEventType deleteContractDefClass

-- |Check if an event is a @CreateContract@ event.
isCreateContractEvent :: Event -> Bool
isCreateContractEvent = isEventType createContractClass

-- |Check if an event is a @UpdateContract@ event.
isUpdateContractEvent :: Event -> Bool
isUpdateContractEvent = isEventType updateContractClass

-- |Check if an event is a @DeleteContract@ event.
isDeleteContractEvent :: Event -> Bool
isDeleteContractEvent = isEventType deleteContractClass

-- |Check if an event is a @ConcludeContract@ event.
isConcludeContractEvent :: Event -> Bool
isConcludeContractEvent = isEventType concludeContractClass

-- |Check if an event is a @TransactionEvent@ event.
isTransactionEvent :: Event -> Bool
isTransactionEvent = isEventType transactionEventClass

extractField' f x = either (const Nothing) Just (extractField f x)

-- |Extract internal time stamp from an event.
extractInternalTimeStamp :: Event -> Maybe DateTime
extractInternalTimeStamp event = do
  v <- fmap unTerm (extractField' eventFieldInternalTimeStamp event)
  case v of
    VDateTime dt -> return dt
    _ -> Nothing

extractDataDefs :: Event -> Maybe [String]
extractDataDefs event = do
  v <- extractField' addDataDefsFieldDefs event
  case project v of
    Just (VList l) -> mapM (\v -> case project v of
                                    Just (VString s) -> return s
                                    _                -> Nothing) l
    _ -> Nothing

-- |Extract report name from a @CreateReport@, @UpdateReport@, or @DeleteReport@
-- event.
extractReportName :: Event -> Maybe String
extractReportName event = do
  v <- extractField' reportFieldName event
  case project v of
    Just (VString s) -> return s
    _ -> Nothing

-- |Extract report description from a @CreateReport@ or @UpdateReport@ event.
extractReportDescription :: Event -> Maybe String
extractReportDescription event = do
  v <- extractField' putReportFieldDescription event
  case project v of
    Just (VString s) -> return s
    _ -> Nothing

-- |Extract report tags from a @CreateReport@ or @UpdateReport@ event.
extractReportTags :: Event -> Maybe [String]
extractReportTags event = do
  v <- extractField' putReportFieldTags event
  case project v of
    Just (VList l) -> mapM (\v -> case project v of Just (VString s) -> Just s
                                                    _ -> Nothing) l
    _ -> Nothing

-- |Extract report code from a @CreateReport@ or @UpdateReport@ event.
extractReportParrotCode :: Event -> Maybe String
extractReportParrotCode event = do
  v <- extractField' putReportFieldParrotCode event
  case project v of
    Just (VString s) -> return s
    _ -> Nothing

-- |Extract reference from a @CreateEntity@, @UpdateEntity@, or @DeleteEntity@
-- event.
extractEntity :: Event -> Maybe VEntity
extractEntity event = do
  v <- extractField' entityFieldEntity event
  case project v of
    Just (VEnt ref) -> return ref
    _ -> Nothing

-- |Extract entity data from a @CreateEntity@ or @UpdateEntity@ event.
extractEntityData :: Event -> Maybe Value
extractEntityData = extractField' putEntityFieldData

-- |Extract entity type from a @CreateEntity@ event.
extractEntityType :: Event -> Maybe String
extractEntityType event = do
  v <- extractField' createEntityFieldType event
  case project v of
    Just (VString s) -> return s
    _ -> Nothing

-- |Extract contract definition name from a @CreateContractDef@,
-- @UpdateContractDef@, or @DeleteContractDef@ event.
extractContractDefName :: Event -> Maybe String
extractContractDefName event = do
  v <- extractField' contractDefFieldName event
  case project v of
    Just (VString name) -> return name
    _ -> Nothing

-- |Extract contract definition code from a @CreateContractDef@ or
-- @UpdateContractDef@ event.
extractContractDefCSLCode :: Event -> Maybe String
extractContractDefCSLCode event = do
  v <- extractField' putContractDefFieldCSLCode event
  case project v of
    Just (VString code) -> return code
    _ -> Nothing

-- |Extract contract ID from a @CreateContract@, @UpdateContract@,
-- @DeleteContract@, or @ConcludeContract@ event.
extractContractId :: Event -> Maybe Int
extractContractId event = do
  v <- extractField' contractEventFieldContractId event
  case project v of
    Just (VInt id) -> return id
    _ -> Nothing

-- |Extract contract meta data from a @CreateContract@ or @UpdateContract@
-- event.
extractContractMetaData :: Event -> Maybe Value
extractContractMetaData = extractField' putContractFieldMetaData

-- |Extract time stamp from a @TransactionEvent@ event.
extractTimeStamp :: Event -> Maybe DateTime
extractTimeStamp event = do
  v <- extractField' transactionEventFieldTimeStamp event
  case project v of
    Just (VDateTime dt) -> return dt
    _ -> Nothing

-- |Extract transaction data from a @TransactionEvent@ event.
extractTransaction :: Event -> Maybe Value
extractTransaction = extractField' transactionEventFieldTransaction