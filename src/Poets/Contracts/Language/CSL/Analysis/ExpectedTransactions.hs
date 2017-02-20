{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts.Language.CSL.Analysis.ExpectedTransactions
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines very simple analysis for CSL clauses: return the set
-- of currently expected transactions. Since obligations/external choices
-- may expect a potentially infinite set of transactions, we collect such
-- classes of transactions in 'TransactionPattern's.
--
--------------------------------------------------------------------------------

module Poets.Contracts.Language.CSL.Analysis.ExpectedTransactions
    (
     TransactionPattern(..),
     Constraint,
     TransactionKind(..),
     getTransactionPatterns
    ) where

import           Control.Monad.Error                              hiding (mapM)
import           Data.Comp.Ops
import           Data.Comp.Sum
import           Data.Comp.Variables
import qualified Data.Map                                         as Map
import           Poets.Contracts.Base
import           Poets.Contracts.Language.CSL.AST
import           Poets.Contracts.Language.CSL.AST.RecordFieldName
import           Poets.Contracts.Language.CSL.Exceptions
import           Poets.Contracts.Language.CSL.Interpreter
import qualified Poets.Contracts.Language.CSL.Interpreter         as I
import           Poets.Data                                       hiding
                                                                   (RunTimeError)
import           Poets.EventLog                                   (Event)
import           Prelude                                          hiding (EQ)

-- |A transaction pattern represents a set of transactions.
data TransactionPattern = TransactionPattern{
      -- |A text describing the set of transactions.
      transactionPatternDescription     :: String,
      -- |A transaction is a response to either an obligation or an external
      -- choice.
      transactionPatternKind            :: TransactionKind,
      -- |The transaction type.
      transactionPatternTransactionType :: TransactionType,
      -- |The constraint to fulfil.
      transactionPatternConstraint      :: Constraint,
      -- |The transaction deadline.
      transactionPatternDeadline        :: (DateTime, DateTime)
    }

-- |A transaction is a response to either an obligation or an external choice.
-- Obligations are annotated with whom is responsible.
data TransactionKind = TKObligation Party
                     | TKExternalChoice

-- |A constraint is an ordinary CSL expression extended with field names.
type Constraint = Term ConExprSig

-- |The signature for constraint expressions.
type ConExprSig = Val :+: VUnit :+: CoreExp :+: RecordFieldName

-- |Get a list of expected transactions for a given contract.
getTransactionPatterns :: SubTypeRelation -- ^Sub type relation.
                       -> [Event] -- ^The event log.
                       -> Predefined -- ^Predefined values/expressions.
                       -> Contract Closure
                       -> Either CSLError (Either Breach [TransactionPattern])
getTransactionPatterns isSubType events pDef c = do
  -- First update the clause
  r <- I.unfold isSubType events pDef c
  case r of
    Left v ->
        return $ Left v
    Right c' ->
        liftM Right $ getTPs' c' isSubType events pDef' (contractLastUpdate c)
    where pDef' = pDef{preExps = closureFunctionDefs (contractContent c)
                                 `Map.union` preExps pDef}

class GetTPs f g where
    getTPs :: f (Term g) -> SubTypeRelation -> [Event] -> Predefined
           -> DateTime -> Either CSLError [TransactionPattern]

instance (GetTPs f h, GetTPs g h) => GetTPs (f :+: g) h where
    getTPs (Inl x) = getTPs x
    getTPs (Inr x) = getTPs x

getTPs' :: GetTPs f f => Term f -> SubTypeRelation -> [Event] -> Predefined
        -> DateTime -> Either CSLError [TransactionPattern]
getTPs' c = getTPs (unTerm c)

-- |Small-step instance for (core) clauses.
instance (CoreClause :<: c, VUnit :<: c, Val :<: c, CoreExp :<: c, (VUnit :+: Val :+: CoreExp) :<: c, ExprCoreSig :<: c, Traversable c, GetTPs c c)
    => GetTPs CoreClause c where
    getTPs c isSubType events pDef baseTime =
        case c of
          Fulfilment{} ->
            return []
          Obligation{clauseResponsible = r,
                     clauseTransactionType = trType,
                     clauseBinders = b,
                     clausePredicate = p,
                     clauseDeadline = d} -> do
            a <- evalExpr' isSubType events pDef r
            (dt1, dt2) <- evalDeadline isSubType events pDef baseTime d
            let p' = normalizeExpr isSubType (preVals pDef events)
                                   (preExps pDef) (substFields b p)
            return [TransactionPattern{
                      transactionPatternDescription = "",
                      transactionPatternKind = TKObligation a,
                      transactionPatternTransactionType = trType,
                      transactionPatternConstraint = p',
                      transactionPatternDeadline = (dt1,dt2)}]
          ExternalChoice{clauseTransactionType = trType,
                         clauseBinders = b,
                         clausePredicate = p,
                         clauseDeadline = d,
                         clauseElse = c} -> do
            (dt1, dt2) <- evalDeadline isSubType events pDef baseTime d
            tps <- getTPs' c isSubType events pDef (max baseTime dt2)
            let p' = normalizeExpr isSubType (preVals pDef events)
                                   (preExps pDef) (substFields b p)
            return $ TransactionPattern{
                         transactionPatternDescription = "",
                         transactionPatternKind = TKExternalChoice,
                         transactionPatternTransactionType = trType,
                         transactionPatternConstraint = p',
                         transactionPatternDeadline = (dt1,dt2)} : tps
          InternalChoice{} ->
            unexpectedInternalChoiceError
          And{clauseLeft = c1, clauseRight = c2} -> do
            tps1 <- getTPs' c1 isSubType events pDef baseTime
            tps2 <- getTPs' c2 isSubType events pDef baseTime
            return $ tps1 ++ tps2
          Or{clauseLeft = c1, clauseRight = c2} -> do
            tps1 <- getTPs' c1 isSubType events pDef baseTime
            tps2 <- getTPs' c2 isSubType events pDef baseTime
            return $ tps1 ++ tps2
          Case{} ->
            unexpectedCaseError
          Instantiate{} ->
            unexpectedInstantiateError
          -- substFields puts in the field names instead of the variables bound
          -- to the field names
          where --substFields :: [(Var,FieldName)] -> Term c -> Constraint
                substFields :: (VUnit :<: f, Val :<: f, CoreExp :<: f,  (VUnit :+: Val :+: CoreExp) :<: f, Traversable f) => [(Var,FieldName)] -> Term f -> Constraint
                substFields b e =
                    let s :: Subst ConExprSig Var = Map.map iRecordFieldName $ Map.fromList b
                        Just (e' :: Term (VUnit :+: Val :+: CoreExp)) = appSigFunM proj $ e
                        e'' :: Term ConExprSig = deepInject e'
                    in appSubst s e''

-- Dummy instance
instance GetTPs VUnit f where
    getTPs _ _ _ _ _ = runTimeError "Unexpected Unit in getTPs"

-- Dummy instance
instance GetTPs Val f where
    getTPs _ _ _ _ _ = runTimeError "Unexpected Val in getTPs"

-- Dummy instance
instance GetTPs CoreExp f where
    getTPs _ _ _ _ _ = runTimeError "Unexpected CoreExp in getTPs"


--------------------------------------------------------------------------------
-- Error messages
--------------------------------------------------------------------------------

-- Throw a runtime error
runTimeError :: String -> Either CSLError a
runTimeError = throwError . RunTimeError

unexpectedInternalChoiceError =
    runTimeError "Error in 'getTPs': Unexpected 'if' clause"

unexpectedCaseError =
    runTimeError "Error in 'getTPs': Unexpected 'case' clause"

unexpectedInstantiateError =
    runTimeError "Error in 'getTPs': Unexpected template instantiation"
