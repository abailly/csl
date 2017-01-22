{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts.Language.CSL
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines CSL as an instance of a contract language via the type
-- class 'ContractLanguage'. Besides the mandatory functionality of any contract
-- language, CSL implements functionality for (a) reading contract/clause
-- definitions (b) extract a set of expected transactions for a contract in the
-- repository, and (c) check whether a contract can be concluded.
--
--------------------------------------------------------------------------------
module Poets.Contracts.Language.CSL
    (
     TransactionPattern(..),
     Constraint,
     TransactionKind(..),
     Input(..),
     Definitions(..),
     Predefined(..),
     VarEnv,
     VarEvalEnv,
     SubTypeRelation,
     module Poets.Contracts.Language.CSL.Exceptions,
     module Poets.Contracts.Language.CSL.AST,
     builtInPredefined,
     addPrelude,
     readContractDefinition,
     getExpectedTransactions,
     isConcludable,
     subTypeRel,
     simpleTyping
    ) where

import Poets.Data hiding (ParseError, Type, Value)
import Poets.EventLog (Event)
import Poets.Contracts.Base
import Poets.Contracts.Language.CSL.AST
import Poets.Contracts.Language.CSL.BuiltIn
import Poets.Contracts.Language.CSL.Desugar
import Poets.Contracts.Language.CSL.Exceptions
import Poets.Contracts.Language.CSL.Interpreter as I
import Poets.Contracts.Language.CSL.TypeChecker
import Poets.Contracts.Language.CSL.Typing.TypeInferer
import Poets.Contracts.Language.CSL.Typing.Inference
import Poets.Contracts.Language.CSL.Analysis.Guardedness
import Poets.Contracts.Language.CSL.Analysis.Concludable (concludable)
import Poets.Contracts.Language.CSL.Analysis.ExpectedTransactions
import Poets.Contracts.Language.CSL.Parser
import Control.Monad.Error
import qualified Data.Set as Set
import qualified Data.Map as Map

-- |Definition environment for contracts in CSL.
data Definitions = Definitions{
      -- |Contract definitions. A contract definition consists of a set of
      -- function definitions and a set of clause definitions.
      defContractDefs :: ContractDefCores,
      -- |Predefined values/expressions and their types.
      defPredefined :: (Predefined, VarEnv)
    }

-- |Additional input needed to define CSL as an instance of 'ContractLanguage'.
data Input = Input{
      inDefs :: Definitions,
      inRecEnv :: RecEnv,
      inEvents :: [Event]
    }

-- |Define CSL as an instance of a contract language.
instance ContractLanguage Closure Input CSLError where
    register inp dt tr c = do
      let cl = contractContent c
      r <- stepC (subTypeRel $ inRecEnv inp)
                 (inEvents inp)
                 (fst $ defPredefined $ inDefs inp) dt tr c
      -- Check if transaction was expected
      unless (fst r) (throwError $ UnexpectedTransaction tr)
      -- Check if contract was breached
      checkBreach (\x -> c{contractContent = cl{closureClause = x},
                           contractLastUpdate = dt}) (snd r)
    instantiate inp m cId = do
      r <- I.instantiate (subTypeRel $ inRecEnv inp)
                         (defContractDefs $ inDefs inp) m cId
      -- Check if contract is immediately breached
      checkBreach id r

-- |The sub type relation induced by a record environment.
subTypeRel :: RecEnv -> SubTypeRelation
subTypeRel recordEnv r1 r2 = either (const False) id $ isSubType recordEnv r1 r2

-- |Propagate breaches as exceptions.
checkBreach :: (a -> b) -> Either Breach a -> Either CSLError b
checkBreach f =
    either (\b -> throwError $ ContractBreach (breachTime b) (breachParties b))
           (return . f)

-- |A set of built-in functions and their typings.
builtInPredefined :: (Predefined, VarEnv)
builtInPredefined =
    (emptyPredefined{preVals = const $ Map.fromList builtInVals},
     foldl (\env (n,(tp,cs),_ :: Value) ->
                Map.insert n Typing{typingType = tp,
                                    typingPoly = True,
                                    typingTypeConstraints =
                                        Set.fromList $ map (:&: Nothing) cs} env)
           Map.empty
           builtIn)
        where builtInVals = map (\(n, _ :: (Type, [TypeInfConstraints Type]), v) -> (n, v)) builtIn

-- |Read (and type check) a set of prelude function definitions. The resulting
-- definitions are added to an existing set.
addPrelude :: Definitions -- ^Initial definitions.
           -> RecEnv
           -> SourceName -- ^File path.
           -> String -- ^String to parse.
           -> Either CSLError Definitions
addPrelude defs recordEnv filePath cont =
    case fmap (map desugarFunctionDef) mpDefs of
      Left err ->
          throwError $ ParseError err
      Right pDefs ->
          case typeInferFunctionDefinitions recordEnv emptyEntityTypingEnv
                                            preTyping pDefs of
            Left err ->
                throwError err
            Right pEnv ->
                return defs{defPredefined =
                                (pre{preExps = add (preExps pre) pDefs},
                                 pEnv `Map.union` preTyping)}
    where (pre, preTyping) = defPredefined defs
          mpDefs = parseFunctions filePath cont
          add = foldr (\pDef -> Map.insert (functionName pDef)
                                           (stripSrcPosFD pDef))

-- |Remove source positions from a function definition.
stripSrcPosFD :: FunctionDefCorePos -> FunctionDefCore
stripSrcPosFD fDef@FunctionDefinition{functionExp = e} =
    fDef{functionExp = stripA e}

-- |Remove source positions from a clause definition.
stripSrcPosCD :: ClauseCorePosDef -> ClauseCoreDef
stripSrcPosCD cDef@ClauseDefinition{clauseDefBody = c} =
    cDef{clauseDefBody = stripA c}

-- |Remove source positions from a contract definition.
stripSrcPosCD' :: ContractDefCorePos -> ContractDefCore
stripSrcPosCD' cDef@ContractDefinition{contractDefFunDefs = fDefs,
                                       contractDefClauseDefs = cDefs,
                                       contractDefBody = c} =
    cDef{contractDefFunDefs = map stripSrcPosFD fDefs,
         contractDefClauseDefs = Map.map stripSrcPosCD cDefs,
         contractDefBody = stripA c}

-- |Parse (and type check) a contract definition.
readContractDefinition :: Definitions
                       -> RecEnv
                       -> EntityTypingEnv -- ^The entity typing environment.
                       -> String -- ^String to parse.
                       -> Either CSLError ContractDefCore
readContractDefinition defs recordEnv entityEnv code =
    case fmap desugarContractDef mcDef of
      Left err ->
          throwError $ ParseError err
      Right cDef -> do
            -- First type check contract definition
            typeCheckContractDefinition recordEnv entityEnv (snd pre) cDef
            let cDef' = stripSrcPosCD' cDef
            -- Then check that the clause definitions are guarded
            either (throwError . ClauseTemplateNotGuarded)
                   return
                   (guarded $ Map.map (\cdef -> cdef{clauseDefBody = clauseDefBody cdef}) $ contractDefClauseDefs cDef')
            return cDef'
    where pre = defPredefined defs
          mcDef = parseContract "" code

-- |Get the set of expected transactions for a contract.
getExpectedTransactions :: Input
                        -> Contract Closure -- ^The contract.
                        -> Either CSLError [TransactionPattern]
getExpectedTransactions inp c = do
  trps <- getTransactionPatterns rel events pre c
  checkBreach id trps
  where rel = subTypeRel $ inRecEnv inp
        events = inEvents inp
        pre = fst $ defPredefined $ inDefs inp

-- |Check if a contract is concludable.
isConcludable :: Input
              -> Contract Closure -- ^The contract.
              -> Either CSLError Bool
isConcludable inp c = do
  b <- concludable rel events pre c
  checkBreach id b
  where rel = subTypeRel $ inRecEnv inp
        events = inEvents inp
        pre = fst $ defPredefined $ inDefs inp