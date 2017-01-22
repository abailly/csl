{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts.Language.CSL.AST.RecordFieldName
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines the AST for record field names. Record field names are
-- used in the constraints,
-- 'Poets.Contracts.Language.CSL.Analysis.ExpectedTransactions.Constraint',
-- returned by the analysis for expected transactions,
-- 'Poets.Contracts.Language.CSL.Analysis.ExpectedTransactions.getTransactionPatterns'.
--
--------------------------------------------------------------------------------

module Poets.Contracts.Language.CSL.AST.RecordFieldName where

import Data.Comp.Derive

-- |AST for extended expressions with top-level (record) field names.
data RecordFieldName e = RecordFieldName String

$(derive [makeFunctor, makeFoldable, makeTraversable,
          makeEqF, smartConstructors]
         [''RecordFieldName] )