{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
             UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts.Language.CSL.Exceptions
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines the exceptions used in CSL.
--
--------------------------------------------------------------------------------

module Poets.Contracts.Language.CSL.Exceptions
    (
     CSLError(..),
    ) where

import Poets.Data hiding (RunTimeError, ParseError)
import Poets.Contracts.Base
import Poets.Contracts.Language.CSL.Render ()
import Control.Monad.Error
import Text.Parsec
import Text.Parsec.Error

-- |The various errors CSL can produce.
data CSLError =
    -- |Unexpected runtime error (i.e., \"this should not happen\").
    RunTimeError String
    -- |The specified clause template is not guarded.
  | ClauseTemplateNotGuarded TemplateName
    -- |The specified contract template is not found.
  | ContractTemplateNotFound TemplateName
    -- |Mismatch between the type of a contract template and the type of the
    -- supplied contract meta data.
  | ContractTemplateTypeMismatch TemplateName ContractType ContractType
    -- |Unexpected transaction.
  | UnexpectedTransaction Transaction
    -- |Contract is breached.
  | ContractBreach DateTime [(Party, String)]
    -- |Input does not type check.
  | TypeError String (Maybe SourcePos)
    -- |Input does not parse.
  | ParseError ParseError

instance Error CSLError where
    strMsg = RunTimeError

instance Show CSLError where
    show (RunTimeError err) =
        "Runtime error: " ++ err
    show (ClauseTemplateNotGuarded tName) =
        "The clause template '" ++ tName ++ "' is not guarded"
    show (ContractTemplateNotFound tName) =
        "Contract template '" ++ tName ++ "' not found"
    show (ContractTemplateTypeMismatch tName cType1 cType2) =
        "Contract template '" ++ tName ++ "' has type " ++ show cType1 ++
        " and can therefore not be used in a contract of type " ++ show cType2
    show (UnexpectedTransaction tr) =
        "Unexpected transaction: " ++ show tr
    show (ContractBreach dt parties) =
        "Contract violated at time " ++ show dt ++ " by " ++ show parties
    show (TypeError e mpos) =
        "Type error" ++ maybe "" ((" in " ++) . show) mpos ++ ":\n" ++ e
    show (ParseError e) =
        "Parse error in " ++ show (errorPos e) ++ ":" ++ 
        showErrorMessages "or" "unknown parse error"
                          "expecting" "unexpected" "end of input"
                          (errorMessages e)