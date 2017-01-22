{-# LANGUAGE TemplateHaskell, TypeOperators, FlexibleContexts #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts.Language.CSL.AST.Type
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines the AST for CSL types.
--
--------------------------------------------------------------------------------

module Poets.Contracts.Language.CSL.AST.Type
    (
     TypeUnit(..),
     TypeVar(..),
     TypeFunction(..),
     (~>),
     -- * Smart Constructors
     iTUnit,
     iTVar,
     iTFunction
    ) where

import Data.Comp.Derive

-- |Signature for unit type.
data TypeUnit e = TUnit
                  deriving (Eq, Ord)

-- |Signature for type variables.
data TypeVar e = TVar String
                 deriving (Eq, Ord)

-- |Signature for function types.
data TypeFunction e = TFunction e e
                      deriving (Eq, Ord)

$( derive [makeFunctor, makeFoldable, makeTraversable, makeEqF,
           makeOrdF, smartConstructors]
          [''TypeUnit, ''TypeVar, ''TypeFunction] )

-- |Macro for CSL function type.
infixr 7 ~>
a ~> b = iTFunction a b