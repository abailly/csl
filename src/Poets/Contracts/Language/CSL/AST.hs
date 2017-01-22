{-# LANGUAGE FlexibleInstances, TypeOperators #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts.Language.CSL.AST
-- Copyright   :  3gERP, 2012
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines the ASTs for the contract language CSL.
--
--------------------------------------------------------------------------------
module Poets.Contracts.Language.CSL.AST
    (
     module X,
     ContractDef(..),
     ContractDefs,
     FunctionDef(..),
     FunctionDefs,
     ClauseDef(..),
     ClauseDefs,
     Closure(..),
     Residue,
     Breach(..),
     -- * Types
     TypeSig,
     Type,
     RecEnv,
     -- * Values
     ValueSig,
     Value,
     -- * ASTs with syntactic sugar and with source positions
     ExprPosSig,
     ExprPos,
     ClausePosSig,
     ClausePos,
     ContractDefPos,
     ContractDefPoss,
     ClausePosDef,
     ClausePosDefs,
     FunctionDefPos,
     FunctionDefPoss,
     -- * ASTs without syntactic sugar and with source positions
     ExprCoreSig,
     ExprCore,
     ClauseCoreSig,
     ClauseCore,
     ContractDefCore,
     ContractDefCores,
     ClauseCoreDef,
     ClauseCoreDefs,
     FunctionDefCore,
     FunctionDefCores,
     -- * ASTs without syntactic sugar and with source positions
     ExprCorePosSig,
     ExprCorePos,
     ClauseCorePosSig,
     ClauseCorePos,
     ContractDefCorePos,
     ContractDefCorePoss,
     ClauseCorePosDef,
     ClauseCorePosDefs,
     FunctionDefCorePos,
     FunctionDefCorePoss
    ) where

import Data.Map (Map)
import Data.Comp
import Poets.Data.Value (Val, DateTime)
import Poets.Data.Type hiding (Type)
import Poets.Contracts.Base
import Poets.Contracts.Language.CSL.AST.Base as X
import Poets.Contracts.Language.CSL.AST.Exp as X
import Poets.Contracts.Language.CSL.AST.Clause as X
import Poets.Contracts.Language.CSL.AST.Type as X

--------------------------------------------------------------------------------
-- Contract definitions
--------------------------------------------------------------------------------

-- |Contract definition.
data ContractDef e c = ContractDefinition{
      -- |The name of the contract definition.
      contractDefName :: TemplateName,
      -- |The type of the contract definition. The type refers to a record type
      -- defined in the ontology, and it defines the parameters which are
      -- available when instantiating a contract.
      contractDefType :: ContractType,
      -- |A text describing the contract definition.
      contractDefDescription :: String,
      -- |A set of function definitions used in the contract. (The order
      -- matters, so we cannot use a Map).
      contractDefFunDefs :: [FunctionDef e],
      -- |A set of clause definitions used in the contract.
      contractDefClauseDefs :: ClauseDefs c,
      -- |The (parametrized) clause defining the template.
      contractDefBody :: Term c
    }

-- |A set of contract definitions.
type ContractDefs e c = Map TemplateName (ContractDef e c)

-- |Function definition.
data FunctionDef e = FunctionDefinition{
      -- |The name of the definition (a variable name).
      functionName :: Var,
      -- ^The expression defining the function.
      functionExp :: Term e
    } deriving Eq

-- |A set of function definitions.
type FunctionDefs e = Map Var (FunctionDef e)

-- |Clause definition.
data ClauseDef c = ClauseDefinition{
      -- |The name of the clause definition.
      clauseDefName :: TemplateName,
      -- |The named parameters and their types.
      clauseDefParams :: [(Var,Type)],
      -- |The named party parameters and their types.
      clauseDefPartyParams :: [(Var,Type)],
      -- |The (parametrized) clause defining the template.
      clauseDefBody :: Term c
    } deriving Eq

-- |A set of clause definitions.
type ClauseDefs c = Map TemplateName (ClauseDef c)

-- |Types in CSL are regular POETS types extended with type variables and
-- function types.
type TypeSig = TypeConstant :+: TypeList :+: TypeEnt :+: TypeUnit :+:
               TypeVar :+: TypeFunction
type Type = Term TypeSig

type RecEnv = RecordEnv Type

-- |Values (signature).
type ValueSig = Val :+: VUnit :+: VFun

-- |Values.
type Value = Term ValueSig

-- Signatures with source positions and with syntactic sugar
type ExprPosSig = Val :&: SrcPos :+: VUnit :&: SrcPos :+:
                  CoreExp :&: SrcPos :+: SugExp :&: SrcPos
type ExprPos = Term ExprPosSig
type ClausePosSig = CoreClause :&: SrcPos :+: SugClause :&: SrcPos :+:
                    ExprPosSig
type ClausePos = Term ClausePosSig
type ContractDefPos = ContractDef ExprPosSig ClausePosSig
type ContractDefPoss = ContractDefs ExprPosSig ClausePosSig
type ClausePosDef = ClauseDef ClausePosSig
type ClausePosDefs = ClauseDefs ClausePosSig
type FunctionDefPos = FunctionDef ExprPosSig
type FunctionDefPoss = FunctionDefs ExprPosSig

-- Signatures without source positions and without syntactic sugar
type ExprCoreSig = Val :+: VUnit :+: CoreExp
type ExprCore = Term ExprCoreSig
type ClauseCoreSig = CoreClause :+: ExprCoreSig
type ClauseCore = Term ClauseCoreSig
type ContractDefCore = ContractDef ExprCoreSig ClauseCoreSig
type ContractDefCores = ContractDefs ExprCoreSig ClauseCoreSig
type ClauseCoreDef = ClauseDef ClauseCoreSig
type ClauseCoreDefs = ClauseDefs ClauseCoreSig
type FunctionDefCore = FunctionDef ExprCoreSig
type FunctionDefCores = FunctionDefs ExprCoreSig

-- Signatures with source positions and without syntactic sugar
type ExprCorePosSig = Val :&: SrcPos :+: VUnit :&: SrcPos :+: CoreExp :&: SrcPos
type ExprCorePos = Term ExprCorePosSig
type ClauseCorePosSig = CoreClause :&: SrcPos :+: ExprCorePosSig
type ClauseCorePos = Term ClauseCorePosSig
type ContractDefCorePos = ContractDef ExprCorePosSig ClauseCorePosSig
type ContractDefCorePoss = ContractDefs ExprCorePosSig ClauseCorePosSig
type ClauseCorePosDef = ClauseDef ClauseCorePosSig
type ClauseCorePosDefs = ClauseDefs ClauseCorePosSig
type FunctionDefCorePos = FunctionDef ExprCorePosSig
type FunctionDefCorePoss = FunctionDefs ExprCorePosSig

--------------------------------------------------------------------------------
-- Contract instances/closures (running contracts)
--------------------------------------------------------------------------------

-- |A closure represents an instance of a clause definition. This amount to the
-- actual clause, and any clause/function definitions that might be needed
-- during run-time monitoring.
data Closure = Closure{
      closureClause :: ClauseCore,
      closureClauseDefs :: ClauseCoreDefs,
      closureFunctionDefs :: FunctionDefCores
    }

-- |The result of run-time monitoring is either a breach, or an updated clause.
type Residue c = Either Breach (Term c)

-- |A breach comprises the time of breach and the parties responsible (annotated
-- with a description of what went wrong).
data Breach = Breach{
      breachTime :: DateTime,
      breachParties :: [(Party, String)]
    }