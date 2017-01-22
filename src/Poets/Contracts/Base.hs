{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FlexibleContexts, 
  TypeOperators #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts.Base
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines base contract definitions that should be present
-- no matter what contract language is used.
--
--------------------------------------------------------------------------------
module Poets.Contracts.Base
    (
     ContractLanguage(..),
     ContractId,
     Contract(..),
     ContractType,
     ContractMetaData,
     Party,
     Transaction,
     TransactionEvent,
     TransactionType,
     TemplateName,
     ContractEngineError(..),
     contractClass,
     extractStartDate,
     extractTemplateName,
     extractContractType,
     extractTransactionType,
     typeCheckContractMetaData,
     typeCheckTransaction
    ) where

import Poets.Data
import Poets.Data.Render
import Poets.EventLog (LogError)
import Control.Monad.Error

-- |Abstract definition of a contract language. The type @a@ represents the
-- contract terms/clauses; the type @b@ represents any auxiliary data that might
-- be needed for registering transactions and instantiation of new contracts;
-- and the type @e@ represents any errors that can occur.
class Error e => ContractLanguage a b e where
    -- |Register a transaction against a contract. Returns the updated contract.
    register :: b -> DateTime
             -> Transaction
             -> Contract a
             -> Either e (Contract a)
    -- |Instantiate a new contract.
    instantiate :: b -> ContractMetaData -> ContractId -> Either e (Contract a)

-- |A contract consists of contract meta data, the actual normative content of
-- the contract (the type depends on the contract language at hand), and a
-- time stamp representing the last update to the contract: in this way we can
-- make sure that transactions are only registered with non-decreasing
-- timestamps.
data Contract a = Contract{
      contractMetaData :: ContractMetaData,
      contractLastUpdate :: DateTime,
      contractContent :: a
    }

-- |Contract /meta data/ (or header information) is a POETS record
-- (the record type must extend the special 'contractClass' record type).
type ContractMetaData = Value

-- |All running contracts are identified with a unique ID.
type ContractId = Int

-- |A transaction type is a record name (which must extend the special
-- 'transactionClass' record type).
type TransactionType = RecordName

-- |Contract /parties/ are POETS values.
type Party = Value

-- |Contract templates are identified by a name.
type TemplateName = String

-- |A contract type is a record name (which must extend the
-- 'contractClass' class).
type ContractType = RecordName

-- |A transaction is a POETS record (the record type must extend the special
-- 'transactionClass' record type).
type Transaction = Value

-- |A transaction event consists of the ID of the contract to which the 
-- transaction belongs, the time stamp of the transaction, and the transaction
-- itself.
type TransactionEvent = (ContractId, DateTime, Transaction)

-- |The various errors the contract engine can produce (parametrized over the
-- errors the chosen contract language might produce).
data ContractEngineError e =
    -- |The specified contract definition already exists.
    ContractDefAlreadyExists TemplateName
    -- |The specified contract definition is not found.
  | ContractDefNotFound TemplateName
    -- |The specified contract is not found.
  | ContractNotFound ContractId
    -- |Tried to register a transaction against a contract with a time stamp in
    -- the past (the first 'DateTime' is the time stamp of the transaction, the
    -- last 'DateTime' is the earliest allowed time stamp).
  | TransactionTooOld ContractId DateTime DateTime
    -- |The specified contract cannot be concluded.
  | ContractNotConcludable ContractId
    -- |Mismatch between the type of a running contract and the type of the
    -- supplied contract meta data for the updated contract.
  | ContractUpdateTypeMismatch ContractId ContractType ContractType
    -- |Error when communicating with the event log.
  | EventLogError LogError
    -- |Error in the contract language, e.g., error when registering a
    -- transaction.
  | ContractLanguageError (Maybe ContractId) e

instance (Error e) => Error (ContractEngineError e) where
    strMsg s = ContractLanguageError Nothing (strMsg s)

instance (Show e) => Show (ContractEngineError e) where
    show (ContractDefAlreadyExists name) =
        "Contract definition " ++ name ++ ": Already exists"
    show (ContractDefNotFound name) =
        "Contract definition " ++ name ++ ": Not found"
    show (ContractNotFound cId) =
        "Contract " ++ show cId ++ ": Not found"
    show (TransactionTooOld cId t1 t2) =
        "Contract " ++ show cId ++ ": Transaction has timestamp " ++
        show t1 ++ ", but transactions are not allowed before " ++ show t2
    show (ContractNotConcludable cId) =
        "Contract " ++ show cId ++ ": Is not concludable"
    show (ContractUpdateTypeMismatch cId cType1 cType2) =
        "Contract " ++ show cId ++ ": Running contract has type '" ++
        show cType1 ++
        " and can therefore not be updated to a contract of type " ++
        show cType2
    show (EventLogError logError) =
        "Event log error: " ++ show logError
    show (ContractLanguageError mCId e) =
        maybe "" (\cId -> "Contract " ++ show cId ++ ": ") mCId ++ show e


--------------------------------------------------------------------------------
-- Fields/records that must be defined in the data model for contracts
--------------------------------------------------------------------------------

-- |The super class for contracts.
contractClass :: RecordName
contractClass = "Contract"

-- |The super class for transactions (e.g., Payment, Delivery, etc.).
transactionClass :: RecordName
transactionClass = "Transaction"

-- |The contract template specified in the meta data of a contract.
contractFieldTemplateName :: FieldName
contractFieldTemplateName = "templateName"

-- |The start date specified in the meta data of a contract.
contractFieldStartDate :: FieldName
contractFieldStartDate = "contractStartDate"


--------------------------------------------------------------------------------
-- Functions for accessing contract meta data and transaction data
--------------------------------------------------------------------------------

-- |Extract the type of the transaction.
extractTransactionType ::Transaction -> Either String TransactionType
extractTransactionType tr = liftM vrecordName (extractRecord tr)

-- |Extract the contract template specified in the meta data of a contract.
extractTemplateName :: ContractMetaData -> Either String TemplateName
extractTemplateName m =
    case fmap unTerm (extractField contractFieldTemplateName m) of
      Right (VString s) -> return s
      _ -> Left "Failed to extract the template name from contract meta data"

-- |Extract the start date specified in the meta data of a contract.
extractStartDate :: ContractMetaData -> Either String DateTime
extractStartDate m =
    case fmap unTerm (extractField contractFieldStartDate m) of
      Right (VDateTime dt) -> return dt
      _ -> Left "Failed to extract the start date from contract meta data"

-- |Extract the contract type defined specified in the meta data of a contract.
extractContractType :: ContractMetaData -> Either String ContractType
extractContractType m = liftM vrecordName (extractRecord m)


--------------------------------------------------------------------------------
-- Type checking of contract meta data and transactions
--------------------------------------------------------------------------------

-- |Type check a POETS value as contract meta data w.r.t. a record typing
-- environment.
typeCheckContractMetaData :: (TypeConstant :<: t, TypeList :<: t, TypeEnt :<: t,
                              EqF t, Functor t, Render t) =>
                             RecordEnv (Term t) -- ^The record typing environment.
                          -> EntityTypingEnv -- ^The entity typing environment.
                          -> ContractMetaData -- ^The meta data to type check.
                          -> Either String ()
typeCheckContractMetaData recordEnv entityEnv =
    typeCheckRecord recordEnv entityEnv contractClass

-- |Type check a POETS value as a transaction w.r.t. a record typing
-- environment.
typeCheckTransaction :: (TypeConstant :<: t, TypeList :<: t, TypeEnt :<: t,
                         EqF t, Functor t, Render t) =>
                        RecordEnv (Term t) -- ^The record typing environment.
                     -> EntityTypingEnv -- ^The entity typing environment.
                     -> Transaction -- ^The transaction to type check.
                     -> Either String ()
typeCheckTransaction recordEnv entityEnv =
    typeCheckRecord recordEnv entityEnv transactionClass
