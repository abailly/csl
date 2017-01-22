{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines the interface to the contract engine. All running
-- contracts are kept in-memory, and only contract meta data is persisted
-- in the event log, 'EventLog'. Upon restart of the server, the contracts are
-- updated based on the persisted event log, thereby omitting the need to
-- persist the state of the contract.
--
-- Currently set up to use the contract language \"CSL\".
--
--------------------------------------------------------------------------------
module Poets.Contracts
    (
     Party,
     ContractId,
     Contract(..),
     Closure(..),
     ClauseCore,
     ContractEngineError(..),
     CSLError(..),
     Transaction,
     TransactionEvent,
     ContractMetaData,
     TransactionPattern(..),
     Constraint,
     TransactionKind(..)
    ) where

import           Control.Monad.Error
import           Data.Char                                       (toLower)
import           Data.IORef
import           Data.List                                       (intersperse)
import qualified Data.Map                                        as Map
import qualified Data.Set                                        as Set
import           Poets.Conc
import           Poets.Contracts.Base
import           Poets.Contracts.Language.CSL                    hiding (getExpectedTransactions)
import qualified Poets.Contracts.Language.CSL                    as CSL
import           Poets.Contracts.Language.CSL.Render
import           Poets.Contracts.Language.CSL.Typing.TypeInferer
import           Poets.Contracts.Repository                      hiding
                                                                  (getContract)
import qualified Poets.Contracts.Repository                      as R
import           Poets.Data                                      hiding (Type,
                                                                  Value)
import           Poets.EntityStore                               hiding
                                                                  (TypeError)
import           Poets.EventLog                                  hiding
                                                                  (TypeError)
import           Poets.Logging
import           Prelude                                         hiding
                                                                  (showList)
