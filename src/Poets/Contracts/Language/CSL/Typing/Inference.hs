{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Contracts.Language.CSL.Typing.Inference
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  : Tom Hvitved
-- Stability   : unknown
-- Portability : unknown
--
-- This module implements type inference for the CSL expression language.
--
--------------------------------------------------------------------------------

module Poets.Contracts.Language.CSL.Typing.Inference
    (
     VarEnv,
     Typing,
     TypeInfConstraints,
     typeInferExpr,
     typeCheckExpr,
     typeInferFunctionDefinitions
    ) where

import           Control.Monad.Error
import           Control.Monad.Reader
import           Data.Comp
import           Data.Comp.Variables
import           Data.Foldable                                   (foldlM)
import           Data.Map                                        (Map)
import qualified Data.Map                                        as Map
import           Data.Maybe
import qualified Data.Set                                        as Set
import           Poets.Contracts.Language.CSL.AST
import           Poets.Contracts.Language.CSL.Exceptions
import           Poets.Contracts.Language.CSL.Typing.Match
import           Poets.Contracts.Language.CSL.Typing.Rename
import           Poets.Contracts.Language.CSL.Typing.Simplify
import           Poets.Contracts.Language.CSL.Typing.TypeInferer hiding (Typing)
import qualified Poets.Contracts.Language.CSL.Typing.TypeInferer as TI
import           Poets.Data                                      hiding (Type)
import           Prelude                                         hiding (EQ, GT,
                                                                  LT, concatMap,
                                                                  mapM, mapM_)

-- We need the following instance in order to infer the type of Expr's, where
-- the source position information is included in the generated constraints
instance (OrdF c, OrdF t) => Ord ((c :&: SrcPos) (Term t)) where
    compare (a :&: _) (b :&: _) = compareF a b

instance TypeInferer f c t => TypeInferer (f :&: SrcPos) c t where
    typeInfAlg (x :&: pos) = local (\env -> env{srcPos = pos}) (typeInfAlg x)

-- |The type constraints that are generated for Expr's (before matching and
-- simplification).
type TypeInfConstraints = SubTypeConstraint :+: FieldConstraint :+:
                          EqConstraint :+: OrderConstraint :+:
                          TrackingConstraint

-- |The variable typing environment used in CSL type inference.
type VarEnv = Map Var Typing

-- |A CSL typing.
type Typing = TI.Typing TypeInfConstraints TypeSig

-- |Infer the type of a CSL expression by constructing a typing (if it exists).
typeInferExpr :: RecEnv
              -> EntityTypingEnv
              -> VarEnv
              -> ExprCorePos -- ^The CSL expression.
              -> Either CSLError Typing
typeInferExpr recEnv entityEnv varEnv e =
    typeInferExpr' recEnv entityEnv varEnv e Nothing

-- |Check the type of a CSL expression by supplying the expected type.
typeCheckExpr :: RecEnv
              -> EntityTypingEnv
              -> VarEnv
              -> ExprCorePos
              -> Type
              -> Either CSLError ()
typeCheckExpr recEnv entityEnv varEnv e expectedTp = do
  typing <- typeInferExpr' recEnv entityEnv varEnv e (Just expectedTp)
  -- TODO: here we need to check that the typing is valid
  return ()

-- |Infer the type of a CSL expression by constructing a typing (if it exists).
-- An optional, expected type can be passed.
typeInferExpr' :: RecEnv
               -> EntityTypingEnv
               -> VarEnv
               -> ExprCorePos -- ^The CSL expression.
               -> Maybe Type -- ^An optional, expected type.
               -> Either CSLError Typing
typeInferExpr' recEnv entityEnv varEnv e expectedTp = do
  -- First perform (basic) type inference
  typing <- either (\(err,p) -> throwError $ TypeError (show err) p)
                   return
                   (typeInfTerm recEnv entityEnv varEnv e)
  let (subcs, fcs, ecs, ocs) = partitionConstraints $ Set.toList $
                               typingTypeConstraints typing
  let pos = snd . projectA $ unTerm e
  -- Add a subtype constraint that the inferred type is a sub type of the
  -- expected type (if such an expected type is passed)
  let subcs2 = maybe subcs
                     (\tp -> (inj (SubTypeConstraint (typingType typing) tp)
                                  :&: pos) : subcs)
                     expectedTp
  -- Then perform matching
  (asubcs,s) <- either (\(MatchError e p) -> throwError $ TypeError e p)
                       return
                       (match subcs2 0)
  -- Then simplification
  (tp1,asubcs1,fcs1,ecs1,ocs1) <- simplify 1
                                           recEnv
                                           pos
                                           (appSubst s $ typingType typing)
                                           asubcs
                                           (appSubst s fcs)
                                           (appSubst s ecs)
                                           (appSubst s ocs)
  let (tp2,cs1) = rename tp1 (mergeConstraints asubcs1 fcs1 ecs1 ocs1)
  return TI.Typing{typingType = tp2,
                   typingPoly = typingPoly typing,
                   typingTypeConstraints = Set.fromList cs1}
      where partitionConstraints :: [(TypeInfConstraints :&: SrcPos) Type]
                                 -> ([(SubTypeConstraint :&: SrcPos) Type],
                                     [(FieldConstraint :&: SrcPos) Type],
                                     [(EqConstraint :&: SrcPos) Type],
                                     [(OrderConstraint :&: SrcPos) Type])
            partitionConstraints cs =
                (mapMaybe (\(c :&: p) -> fmap (:&: p) $ proj c) cs,
                 mapMaybe (\(c :&: p) -> fmap (:&: p) $ proj c) cs,
                 mapMaybe (\(c :&: p) -> fmap (:&: p) $ proj c) cs,
                 mapMaybe (\(c :&: p) -> fmap (:&: p) $ proj c) cs)
            mergeConstraints :: [(SubTypeConstraint :&: SrcPos) AtomicType]
                             -> [(FieldConstraint :&: SrcPos) Type]
                             -> [(EqConstraint :&: SrcPos) Type]
                             -> [(OrderConstraint :&: SrcPos) Type]
                             -> [(TypeInfConstraints :&: SrcPos) Type]
            mergeConstraints scs fcs ecs ocs =
                map (\(c :&: p) -> inj (fmap undefined -- deepInject2
                                        c) :&: p) scs ++
                map (\(c :&: p) -> inj c :&: p) fcs ++
                map (\(c :&: p) -> inj c :&: p) ecs ++
                map (\(c :&: p) -> inj c :&: p) ocs

-- |Infer the type for a set of function definitions in sequential order of
-- appearance, w.r.t. a POETS record environment and an existing typing
-- environment.
typeInferFunctionDefinitions :: RecEnv
                             -> EntityTypingEnv
                             -> VarEnv
                             -> [FunctionDefCorePos]
                             -> Either CSLError VarEnv
typeInferFunctionDefinitions recordEnv entityEnv vEnv =
    foldlM (\fEnv fDef -> do
              tp <- typeInferExpr recordEnv
                                  entityEnv
                                  (fEnv `Map.union` vEnv)
                                  (functionExp fDef)
              return $ Map.insert (functionName fDef) tp fEnv)
           Map.empty
