--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts.Language.CSL.AST.Base
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines basic AST definitions used in CSL.
--
--------------------------------------------------------------------------------

module Poets.Contracts.Language.CSL.AST.Base
    (
     SrcPos
    ) where

import Text.Parsec.Pos

-- |Source position information.
type SrcPos = Maybe SourcePos