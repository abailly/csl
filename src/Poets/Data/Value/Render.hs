--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Data.Value.Render
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved, Patrick Bahr, and Morten Ib Nielsen
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines rendering of values.
--
--------------------------------------------------------------------------------

module Poets.Data.Value.Render
    (
     printVal,
     module Poets.Data.Render
    ) where

import Text.PrettyPrint.Leijen
import Poets.Data.Value
import Poets.Data.Render

-- |In order to avoid overlapping instances, we add an explicit printing of
-- POETS values, which uses the 'Render' instance of this module.
printVal :: Value -> String
printVal = printOneLine . renderCompact . renderTerm
    where printOneLine :: SimpleDoc -> String
          printOneLine SEmpty = ""
          printOneLine (SChar c x) = c : printOneLine x
          printOneLine (SText _ s x) = s ++ printOneLine x
          printOneLine (SLine _ x) = printOneLine x

instance Render Val where
    render (VInt i) =
        int i
    render (VBool True) =
        text "true"
    render (VBool False) =
        text "false"
    render (VString s) =
        dquotes $ text s
    render (VDate d) =
        text $ show d
    render (VTime t) =
        text $ show t
    render (VDateTime dt) =
        text $ show dt
    render (VDuration d) =
        text $ show d
    render (VReal d) =
        double d
    render (VEnt VEntity{ventType = rName, ventId = id, ventContext = mDt}) =
        text rName <>
        angles (int id <> maybe empty (\dt -> comma <> text (show dt)) mDt)
    render (VRecord VR{vrecordName = rName, vrecordFields = fields}) =
        text rName <>
        if fieldsNull fields then
            empty
        else
            encloseSep lbrace rbrace comma $ map renderVField $
                                                 fieldsList fields
            where renderVField vf = text (vfieldName vf) <+>
                                    equals <+>
                                    vfieldValue vf

    render (VList vs) =
        encloseSep lbracket rbracket comma vs