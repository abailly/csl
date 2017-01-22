{-# LANGUAGE TypeOperators, FlexibleContexts, TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts.Language.CSL.AST.Clause
-- Copyright   :  3gERP, 2012
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines the AST for CSL clauses. This includes both the core
-- syntax as well as syntactic sugar.
--
--------------------------------------------------------------------------------

module Poets.Contracts.Language.CSL.AST.Clause
    (
     CoreClause(..),
     SugClause(..),
     Deadline(..),
     DeadlineSug(..),
     ClauseCase(..),
     -- * Smart constructors
     iFulfilment,
     iObligation,
     iExternalChoice,
     iInternalChoice,
     iAnd,
     iOr,
     iCase,
     iInstantiate,
     iObligationSug,
     iExternalChoiceSug,
     iClauseCase,
     iDeadline,
     iImmediately,
     iWithin,
     iAfter,
     -- * Smart constructors w/ annotations
     iAFulfilment,
     iAObligation,
     iAExternalChoice,
     iAInternalChoice,
     iAAnd,
     iAOr,
     iACase,
     iAInstantiate,
     iAObligationSug,
     iAExternalChoiceSug,
     iAClauseCase,
     iADeadline,
     iAImmediately,
     iAWithin,
     iAAfter
    ) where

import Poets.Data.Value (RecordName, FieldName)
import Poets.Contracts.Base
import Poets.Contracts.Language.CSL.AST.Exp
import Data.Comp.Derive

-- |The signature for CSL (core) clauses.
data CoreClause e =
    -- |The trivially fulfiled clause.
    Fulfilment
    -- |Obligation.
  | Obligation{
      -- |The responsible party.
      clauseResponsible :: e,
      -- |The type of the obligation.
      clauseTransactionType :: TransactionType,
      -- |Association of variables with fields defined for the transaction type.
      clauseBinders :: [(Var,FieldName)],
      -- |The name of the /remainder variable/.
      clauseRemainderVar :: Var,
      -- |The predicate to fulfil.
      clausePredicate :: e,
      -- |The deadline to fulfil.
      clauseDeadline :: Deadline e,
      -- |The continuation clause.
      clauseContinuation :: e
    }
    -- |External choice.
  | ExternalChoice{
      clauseTransactionType :: TransactionType,
      clauseBinders :: [(Var,FieldName)],
      clauseRemainderVar :: Var,
      clausePredicate :: e,
      clauseDeadline :: Deadline e,
      clauseContinuation :: e,
      -- |The /else/ branch.
      clauseElse :: e
    }
    -- |Internal choice.
  | InternalChoice{
      -- |The branch condition.
      clauseCondition :: e,
      -- |The /then/ clause.
      clauseThen :: e,
      -- |The /else/ clause.
      clauseElse :: e
    }
    -- |Clause conjunction.
  | And{
      -- |The left clause.
      clauseLeft :: e,
      -- |The right clause.
      clauseRight :: e
    }
    -- |Clause disjunction
  | Or{
      clauseLeft :: e,
      clauseRight :: e
    }
    -- |Case split.
  | Case{
      -- |The case expression.
      clauseCaseExpr :: e,
      -- |Type annotation for uniqueness.
      clauseCaseType :: Maybe RecordName,
      -- |The different cases.
      clauseCases :: [ClauseCase e]
    }
    -- |Template instantiation.
  | Instantiate{
      -- |The name of the template to instantiate.
      clauseClauseTemplate :: TemplateName,
      -- |Arguments to the template.
      clauseArgs :: [e],
      -- |Party arguments to the template.
      clausePartyArgs :: [e]
    }

-- |A deadline constitutes two duration expressions (the typechecker will check
-- that the CSL expressions have the right type).
data Deadline e = Deadline{deadlineWithin :: e,
                           deadlineAfter :: e}
                  deriving (Eq, Ord)

-- |A branch in a case clause.
data ClauseCase e = ClauseCase{clauseCaseRecordName :: RecordName,
                               clauseCaseVar :: Var,
                               clauseCaseBody :: e}
                    deriving (Eq, Ord)

-- |The signature for CSL syntactic sugar clauses.
data SugClause e =
    -- |Obligation.
    ObligationSug{
      -- |The responsible party.
      clauseSugResponsible :: e,
      -- |The type of the obligation.
      clauseSugTransactionType :: TransactionType,
      -- |Association of variables with fields defined for the transaction type.
      clauseSugBinders :: [(Var,FieldName)],
      -- |The name of the /remainder variable/.
      clauseSugRemainderVar :: Maybe Var,
      -- |The predicate to fulfil.
      clauseSugPredicate :: Maybe e,
      -- |The deadline to fulfil.
      clauseSugDeadline :: DeadlineSug e,
      -- |The continuation clause.
      clauseSugContinuation :: Maybe e
    }
    -- |External choice.
  | ExternalChoiceSug{
      clauseSugTransactionType :: TransactionType,
      clauseSugBinders :: [(Var,FieldName)],
      clauseSugRemainderVar :: Maybe Var,
      clauseSugPredicate :: Maybe e,
      clauseSugDeadline :: DeadlineSug e,
      clauseSugContinuation :: Maybe e,
      -- |The /else/ branch.
      clauseSugElse :: Maybe e
    }

data DeadlineSug e = Immediately{deadlineSugAfterM :: Maybe e}
                   | Within{deadlineSugWithin :: e,
                            deadlineSugAfterM :: Maybe e}
                   | After{deadlineSugAfter :: e}
                     deriving (Eq, Ord)

$(derive [makeFunctor, makeFoldable, makeTraversable, makeEqF, makeOrdF,
          smartConstructors, smartAConstructors]
         [''CoreClause, ''SugClause, ''ClauseCase, ''Deadline, ''DeadlineSug])