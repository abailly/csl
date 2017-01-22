{-# LANGUAGE ScopedTypeVariables, TypeOperators, MultiParamTypeClasses,
  FlexibleInstances, FlexibleContexts, TypeSynonymInstances,
  UndecidableInstances, TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts.Language.CSL.Analysis.Guardedness
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- Check that a set of CSL clause templates are guarded, i.e., that the
-- unfolding of a CSL clause always terminates. This effectively rules out
-- \"definitions\" such as @f()\<\> = f()\<\>@ and @f()\<\> = c and f()\<\>@.
--
--------------------------------------------------------------------------------

module Poets.Contracts.Language.CSL.Analysis.Guardedness
    (
     guarded
    ) where

import Data.Comp
import Data.Comp.Ops
import Poets.Data.Value
import Poets.Contracts.Base
import Poets.Contracts.Language.CSL.AST
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
import qualified Data.Map as Map

-- |The guardedness monad. Template/function definitions are stored in the
-- environment, and the call graph is stored in the state.
type GM c = ReaderT (ClauseDefs c) (ErrorT TemplateName (State GState))

-- |The state records the templates that are used in the definition of a
-- template ('gTemplateName').
data GState = GState{gTemplateName :: TemplateName,
                     gCallTrace :: [TemplateName]}

class Guarded f g where
    isG :: Alg f (GM g ())

instance (Guarded f h, Guarded g h) => Guarded (f :+: g) h where
    isG (Inl x) = isG x
    isG (Inr x) = isG x

isGuarded :: (Functor f, Guarded f f) => Term f -> GM f ()
isGuarded = cata isG

instance (Functor f, Guarded f f) => Guarded CoreClause f where
    isG Fulfilment{} =
        return ()
    isG Obligation{} =
        return ()
    isG ExternalChoice{clauseElse = cElse} =
        cElse
    isG InternalChoice{clauseThen = c1, clauseElse = c2} = do
        c1
        c2
    isG And{clauseLeft = c1, clauseRight = c2} = do
        c1
        c2
    isG Or{clauseLeft = c1, clauseRight = c2} = do
        c1
        c2
    isG Case{clauseCases = cs} =
        mapM_ clauseCaseBody cs
    isG Instantiate{clauseClauseTemplate = tName} = do
        clauseTemplates <- ask
        state <- get
        case Map.lookup tName clauseTemplates of
          Nothing ->
              -- The clause template was not found, but we do not report
              -- an error, since it is the responsibility of the type
              -- checker
              return ()
          Just clauseTemplate ->
              let trace = gCallTrace state in
              if tName `elem` trace then
                  -- Call graph is not guarded
                  throwError $ gTemplateName state
              else do
                  put state{gCallTrace = tName : trace}
                  isGuarded $ clauseDefBody clauseTemplate

-- Dummy instance
instance Guarded VUnit f where
    isG _ = throwError "Unexpected Unit in isG"

-- Dummy instance
instance Guarded Val f where
    isG _ = throwError "Unexpected Val in isG"

-- Dummy instance
instance Guarded CoreExp f where
    isG _ = throwError "Unexpected CoreExp in isG"

-- |Check that a set of clause templates are guarded. This amounts to checking
-- that each individual template is guarded.
guarded :: (Functor c, Guarded c c) => ClauseDefs c -> Either TemplateName ()
guarded cDefs =
    mapM_ (\c -> fst $ runState
                 (runErrorT $ runReaderT (isGuarded $ clauseDefBody c) cDefs)
                 GState{gTemplateName = clauseDefName c,
                        gCallTrace = []}) $ Map.elems cDefs