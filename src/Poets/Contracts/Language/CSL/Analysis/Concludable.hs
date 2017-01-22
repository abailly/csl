{-# LANGUAGE ScopedTypeVariables, TypeOperators, MultiParamTypeClasses,
  FlexibleInstances, FlexibleContexts, TypeSynonymInstances,
  TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts.Language.CSL.Analysis.Concludable
-- Copyright   :  3gERP, 2012
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- Check if a CSL clause is /concludable/, i.e., if it is fulfilled.
--
--------------------------------------------------------------------------------

module Poets.Contracts.Language.CSL.Analysis.Concludable
    (
     concludable
    ) where

import Data.Comp
import Data.Comp.Derive
import Poets.Data.Value
import Poets.EventLog (Event)
import Poets.Contracts.Base
import Poets.Contracts.Language.CSL.Exceptions
import Poets.Contracts.Language.CSL.AST
import Poets.Contracts.Language.CSL.Interpreter
import qualified Poets.Contracts.Language.CSL.Interpreter as I
import Control.Monad.Error

class Conc f where
    isConc :: Alg f (Either CSLError Bool)

$(derive [liftSum] [''Conc])

-- |Check if a clause is concludable. The function assumes that all top-level
-- template calls and if-then-else clauses are unfolded.
concludable ::  SubTypeRelation -- ^Sub type relation.
            -> [Event] -- ^The event log.
            -> Predefined -- ^Predefined values/expressions.
            -> Contract Closure
            -> Either CSLError (Either Breach Bool)
concludable isSubType events pDef c = do
  -- First update the clause
  r <- I.unfold isSubType events pDef c
  case r of
    Left v ->
        return $ Left v
    Right c' ->
        liftM return $ cata isConc c'

instance Conc CoreClause where
    isConc Fulfilment{} = return True
    isConc Obligation{} = return False
    isConc ExternalChoice{clauseElse = cElse} = cElse
    isConc And{clauseLeft = c1, clauseRight = c2} = liftM2 (&&) c1 c2
    isConc Or{clauseLeft = c1, clauseRight = c2} = liftM2 (||) c1 c2
    isConc _ = throwError $ RunTimeError "unexpected clause in function concludable"

-- Dummy instance
instance Conc VUnit where
    isConc _ = throwError $ RunTimeError "Unexpected Unit in isConc"

-- Dummy instance
instance Conc Val where
    isConc _ = throwError $ RunTimeError "Unexpected Val in isConc"

-- Dummy instance
instance Conc CoreExp where
    isConc _ = throwError $ RunTimeError "Unexpected CoreExp in isConc"