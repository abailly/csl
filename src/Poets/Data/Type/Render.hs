--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Data.Type.Render
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved, Patrick Bahr, and Morten Ib Nielsen
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines rendering of types.
--
--------------------------------------------------------------------------------
module Poets.Data.Type.Render
    (
     module Poets.Data.Render
    )
    where

import Text.PrettyPrint.Leijen
import Poets.Data.Type
import Poets.Data.Render

instance Render TypeConstant where
    render (TInt) = text "Int"
    render (TBool) = text "Bool"
    render (TString) = text "String"
    render (TDate) = text "Date"
    render (TTime) = text "Time"
    render (TDateTime) = text "DateTime"
    render (TDuration) = text "Duration"
    render (TReal) = text "Real"
    render (TRecord rn) = text rn

instance Render TypeEnt where
    render (TEnt rn) = angles rn

instance Render TypeList where
    render (TList t) = text "[" <> t <> text "]"