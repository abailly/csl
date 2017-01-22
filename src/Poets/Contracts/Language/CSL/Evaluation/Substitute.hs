{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts.Language.CSL.Evaluation.Substitute
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines instances of @HasVars@ in order to substitute into CSL
-- expressions.
--
--------------------------------------------------------------------------------
module Poets.Contracts.Language.CSL.Evaluation.Substitute
    (
     Subst,
     appSubst
    ) where

import           Data.Comp.Variables
import qualified Data.Map                                         as Map
import qualified Data.Set                                         as Set
import           Poets.Contracts.Language.CSL.AST.Exp
import           Poets.Contracts.Language.CSL.AST.RecordFieldName
import           Poets.Data.Value

instance HasVars VUnit v where

instance HasVars Val v where

instance HasVars RecordFieldName v where

instance HasVars CoreExp Var where
    isVar EVar{expVar = x} = Just x
    isVar _                = Nothing
    bindsVars (ELambda x b) = b |-> Set.singleton x
    bindsVars _             = empty
