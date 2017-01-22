{-# LANGUAGE TypeOperators, FlexibleContexts, ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts.Repository
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module contains functionality for the (in-memory) contract repository.
-- The implementation is parametrized over the actual contract language used,
-- via the type class 'ContractLanguage'.
--
--------------------------------------------------------------------------------

module Poets.Contracts.Repository
    (
     Repository,
     generateContractId,
     getContract,
     getContracts,
     registerTransaction,
     instantiate,
     update,
     remove
    ) where

import Poets.Contracts.Base hiding (instantiate)
import qualified Poets.Contracts.Base as B
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

-- |The contract repository monad.
type RM a b e = ReaderT b (ErrorT (ContractEngineError e)
                                  (State (Repository a)))

-- |Run a computation in the repository monad.
runRM :: b
      -> Repository a
      -> RM a b e x
      -> Either (ContractEngineError e) (x, Repository a)
runRM env repo m =
    let (x,newRepo) = runState (runErrorT $ runReaderT m env) repo in
    either Left (\x -> Right (x,newRepo)) x

evalRM :: b -> Repository a -> RM a b e x -> Either (ContractEngineError e) x
evalRM env repo m = fmap fst $ runRM env repo m

execRM :: b
       -> Repository a
       -> RM a b e x
       -> Either (ContractEngineError e) (Repository a)
execRM env repo m = fmap snd $ runRM env repo m

-- |A contract repository is a mapping of contract IDs to 'Maybe' contracts and
-- previously logged transactions. (Deleted/concluded contracts are mapped to
-- 'Nothing'.) Logged transactions are stored in /reversed/ order, so newly
-- added transactions can be efficiently cons'ed to the old list.
type Repository a = Map ContractId (Maybe (Contract a, [TransactionEvent]))

-- |Lookup a contract in the repository by ID.
lookupContract :: (ContractLanguage a b e) => ContractId
               -> RM a b e (Contract a, [TransactionEvent])
lookupContract cId = do
  r <- get
  maybe (throwError $ ContractNotFound cId)
        (maybe (throwError $ ContractNotFound cId) return)
        (Map.lookup cId r)

-- |Update a contract in the repository.
updateContract :: (ContractLanguage a b e) => ContractId
               -> (Contract a, [TransactionEvent])
               -> RM a b e ()
updateContract cId (c,trs) = do
  r <- get
  put $ Map.update (\_ -> Just (Just (c,trs))) cId r

-- |Register a transaction in the contract repository.
registerTr :: (ContractLanguage a b e) => TransactionEvent -> RM a b e ()
registerTr trEvent@(cId, timeStamp, tr) = do
  (c,trs) <- lookupContract cId
  -- Check that the transaction is not in the past
  when (timeStamp < contractLastUpdate c)
       (throwError $ TransactionTooOld cId timeStamp $ contractLastUpdate c)
  -- Do the actual registration
  env <- ask
  case register env timeStamp tr c of
    Left err ->
        throwError $ ContractLanguageError (Just cId) err
    Right c ->
        -- Update the contract, and cons the transaction to the 'log'
        updateContract cId (c{contractLastUpdate = timeStamp}, trEvent : trs)

-- |Instantiate a new contract.
instantiateContract :: (ContractLanguage a b e) => ContractId
                    -> ContractMetaData
                    -> RM a b e ()
instantiateContract cId metaData = do
  env <- ask
  case B.instantiate env metaData cId of
    Left err ->
        throwError $ ContractLanguageError (Just cId) err
    Right c -> do
      -- Now add the contract to the repository
      repo <- get
      put $ Map.insert cId (Just (c,[])) repo

-- |Update a /running/ contract. This amounts to creating a new contract (but
-- with the ID of the old contract), and registering any existing transactions
-- against the new (i.e., updated) contract.
updateContract' :: (ContractLanguage a b e) => ContractId
                -> ContractMetaData
                -> RM a b e ()
updateContract' cId metaData = do
  -- Fetch old transactions
  (_,trs) <- lookupContract cId
  env <- ask
  -- Instantiate updated contract
  case B.instantiate env metaData cId of
    Left err ->
        throwError $ ContractLanguageError (Just cId) err
    Right c -> do
      -- Register old transactions against the updated contract (we must
      -- remember to reverse the list to process old transaction in
      -- chronological order).
      updateContract cId (c,[])
      mapM_ registerTr $ reverse trs
      return ()


--------------------------------------------------------------------------------
-- Exported functions
--------------------------------------------------------------------------------

-- |Generate fresh contract ID.
generateContractId :: Repository a -> ContractId
generateContractId repo =
    if Map.null repo then 0 else fst (Map.findMax repo) + 1

-- |Lookup a contract in the repository.
getContract :: (ContractLanguage a b e) => b
            -> Repository a -- ^The contract repository.
            -> ContractId -- ^The ID of the contract to lookup.
            -> Either (ContractEngineError e) (Contract a)
getContract env repo cId = liftM fst $ evalRM env repo (lookupContract cId)

-- |Get a list of all active contracts in the repository.
getContracts :: (ContractLanguage a b e) => b
             -> Repository a -- ^The contract repository.
             -> Either (ContractEngineError e) [Contract a]
getContracts env repo = evalRM env repo lookupContracts
    where lookupContracts = do r <- get
                               return $ map fst $ catMaybes $ Map.elems r

-- |Register a transaction in the contract repository.
registerTransaction :: (ContractLanguage a b e) => b
                    -> Repository a -- ^The contract repository.
                    -> TransactionEvent -- ^The transaction to register.
                    -> Either (ContractEngineError e) (Repository a)
registerTransaction env repo trEvent = execRM env repo $ registerTr trEvent

-- |Instantiate a new contract based on the supplied contract ID and contract
-- meta data. The updated repository is returned.
instantiate :: (ContractLanguage a b e) => b
            -> Repository a -- ^The contract repository.
            -> ContractId -- ^ID of contract to instantiate.
            -> ContractMetaData -- ^Meta data of contract to instantiate.
            -> Either (ContractEngineError e) (Repository a)
instantiate env repo cId metaData =
    execRM env repo (instantiateContract cId metaData)

-- |Update a contract based on the supplied contract ID and contract meta data.
-- The updated repository is returned.
update :: (ContractLanguage a b e) => b
       -> Repository a -- ^The contract repository.
       -> ContractId -- ^ID of contract to update.
       -> ContractMetaData -- ^Updated meta data of contract.
       -> Either (ContractEngineError e) (Repository a)
update env repo cId metaData = execRM env repo (updateContract' cId metaData)

-- |Remove a contract based on the supplied contract ID. The updated repository
-- is returned.
remove :: Repository a -- ^The contract repository.
       -> ContractId -- ^ID of contract to remove.
       -> Repository a
remove repo cId = Map.update (\_ -> Just Nothing) cId repo
