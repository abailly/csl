{-# LANGUAGE TypeOperators, TypeSynonymInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Contracts.Language.CSL.Typing.Rename
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module implements functions for renaming type variables to "pretty"
-- names in typings.
--
--------------------------------------------------------------------------------

module Poets.Contracts.Language.CSL.Typing.Rename
    (
     rename
    ) where

import Data.Comp.Decompose
import Data.Comp.Variables
import Data.Char
import Data.Foldable hiding (notElem)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Poets.Data hiding (Type)
import Poets.Contracts.Language.CSL.AST
import Poets.Contracts.Language.CSL.Typing.Match ()
import Poets.Contracts.Language.CSL.Typing.TypeInferer

buildSubst _ [] = Map.empty
buildSubst c (c1:cs) = if [c] `notElem` (c1:cs) then
                           let subst = buildSubst (chr $ ord c + 1) cs in
                           Map.insert c1 (iTVar [c] :: Type) subst
                       else
                           buildSubst (chr $ ord c + 1) (filter (/= [c]) cs)
                     
                    

-- |Rename existing type variables to /pretty/ type variables, i.e., @a@, @b@,
-- @c@, etc.
rename :: (Foldable c, Functor c) => Type
       -> [(c :&: p) Type]
       -> (Type, [(c :&: p) Type])
rename tp cs =
    -- Type variables occurring in the constraints
    let xs = Set.unions $ map (Set.unions . map variables .
                               arguments . (\(x :&: _) -> x)) cs in
    -- Type variables occurring in the type
    let ys = variables tp :: Set.Set TypeVarId in
    let typeVars = Set.toList $ Set.union xs ys in
    let subst = buildSubst 'a' typeVars in
    (appSubst subst tp, map (fmap (appSubst subst)) cs)