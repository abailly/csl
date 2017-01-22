{-# LANGUAGE TypeSynonymInstances, TypeOperators, FlexibleInstances,
  FlexibleContexts, UndecidableInstances,
  OverlappingInstances, MultiParamTypeClasses #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Contracts.Language.CSL.Desugar
-- Copyright   : 3gERP, 2012
-- License     : All Rights Reserved
--
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- Desugaring of CSL contracts and expressions.
--
--------------------------------------------------------------------------------

module Poets.Contracts.Language.CSL.Desugar
    (
     desugarContractDef,
     desugarClauseDef,
     desugarFunctionDef,
     desugarExpr
    ) where

import Data.Maybe (fromMaybe)
import Data.Comp
import Data.Comp.Derive
import Data.Comp.Desugar
import qualified Data.Map as Map
import Poets.Data.Value
import Poets.Contracts.Language.CSL.AST
import Prelude hiding (LT,EQ,GT)

-- |Desugar a contract definition produced from parsing.
desugarContractDef :: ContractDefPos -> ContractDefCorePos
desugarContractDef cDef@ContractDefinition{contractDefFunDefs = fDefs,
                                           contractDefClauseDefs = cDefs,
                                           contractDefBody = c} =
    cDef{contractDefFunDefs = map desugarFunctionDef fDefs,
         contractDefClauseDefs = Map.map desugarClauseDef cDefs,
         contractDefBody = desugarClause c}

-- |Desugar a clause definition produced from parsing.
desugarClauseDef :: ClausePosDef -> ClauseCorePosDef
desugarClauseDef cDef@ClauseDefinition{clauseDefBody = c} =
    cDef{clauseDefBody = desugarClause c}

-- |Desugar a function definition produced from parsing.
desugarFunctionDef :: FunctionDefPos -> FunctionDefCorePos
desugarFunctionDef fDef@FunctionDefinition{functionExp = e} =
    fDef{functionExp = appHom (propAnn desugHom) e}


--------------------------------------------------------------------------------
-- Desugaring of clauses
--------------------------------------------------------------------------------

instance (CoreClause :<: f, Val :<: f, Functor f) => Desugar SugClause f where
    desugHom' c =
        case c of
          ObligationSug{clauseSugResponsible = r,
                        clauseSugTransactionType = trType,
                        clauseSugBinders = b,
                        clauseSugRemainderVar = mx,
                        clauseSugPredicate = mp,
                        clauseSugDeadline = d,
                        clauseSugContinuation = mc} ->
               inject Obligation{
                            clauseResponsible = r,
                            clauseTransactionType = trType,
                            clauseBinders = b,
                            clauseRemainderVar = fromMaybe dummyVar mx,
                            clausePredicate = fromMaybe (iVBool True) mp,
                            clauseDeadline = desugarD d,
                            clauseContinuation = fromMaybe iFulfilment mc}
          ExternalChoiceSug{clauseSugTransactionType = trType,
                            clauseSugBinders = b,
                            clauseSugRemainderVar = mx,
                            clauseSugPredicate = mp,
                            clauseSugDeadline = d,
                            clauseSugContinuation = mc,
                            clauseSugElse = me} ->
               inject ExternalChoice{
                            clauseTransactionType = trType,
                            clauseBinders = b,
                            clauseRemainderVar = fromMaybe dummyVar mx,
                            clausePredicate = fromMaybe (iVBool True) mp,
                            clauseDeadline = desugarD d,
                            clauseContinuation = fromMaybe iFulfilment mc,
                            clauseElse = fromMaybe iFulfilment me}
        where desugarD Immediately{deadlineSugAfterM = ma} =
                  Deadline{deadlineWithin = d0,
                           deadlineAfter = fromMaybe d0 ma}
              desugarD Within{deadlineSugWithin = w,
                              deadlineSugAfterM = ma} =
                  Deadline{deadlineWithin = w,
                           deadlineAfter = fromMaybe d0 ma}
              desugarD After{deadlineSugAfter = a} =
                  Deadline{deadlineWithin = d0,
                           deadlineAfter = a}
              d0 = iVDuration (fromSeconds 0)

-- |Desugar a clause produced from parsing.
desugarClause :: ClausePos -> ClauseCorePos
desugarClause = appHom (propAnn desugHom)

--------------------------------------------------------------------------------
-- Desugaring of expressions
--------------------------------------------------------------------------------

instance (Val :<: f, CoreExp :<: f, Functor f) => Desugar SugExp f where
    desugHom' ESUnOp{expSugUnOp = op, expSugUnOpArg = e} =
        case op of
          NOT    -> iEIfThenElse e (iVBool False) (iVBool True)
          UMINUS -> iEBinOp TIMES (iVInt (-1)) e
    desugHom' ESBinOp{expSugBinOp = op,
                      expSugBinOpArg1 = e1,
                      expSugBinOpArg2 = e2} =
        case op of
          NEQ    -> desugHom' $ ESUnOp NOT $ iEBinOp EQ e1 e2
          GEQ    -> iEBinOp LEQ e2 e1
          LT     -> iEBinOp AND (iEBinOp LEQ e1 e2) (desugHom' $ ESBinOp NEQ e1 e2)
          GT     -> desugHom' $ ESBinOp LT e2 e1
          MINUS  -> iEBinOp PLUS e1 (iEBinOp TIMES (iVInt $ -1) e2)
          DMINUS -> iEBinOp DPLUS e1 (iEBinOp DTIMES (iVInt $ -1) e2)
          OR     -> let not = desugHom' . ESUnOp NOT in
                    not (iEBinOp AND (not e1) (not e2))
    desugHom' ESLet{expSugLetName = x, expSugLetDef = e1, expSugLetBody = e2} =
        iEApply (iELambda x e2) e1
    desugHom' ESLambda{expSugLambdaParams = params, expSugLambdaBody = body} =
        foldr iELambda body params
    desugHom' ESUpdate{expSugUpdateRecord = x, expSugUpdateList = l} =
        foldl (\e (f,e') -> iEUpdate e f e') x l

desugarExpr :: ExprPos -> ExprCorePos
desugarExpr = appHom (propAnn desugHom)