{-# LANGUAGE DoAndIfThenElse      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Contracts.Language.CSL.Typing.Simplify
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module implements functions for simplifying typings.
--
--------------------------------------------------------------------------------

module Poets.Contracts.Language.CSL.Typing.Simplify
    (
     simplify
    ) where

import           Control.Monad.Error
import qualified Data.Array                                      as Array
import           Data.Comp.Variables
import           Data.Foldable                                   (toList)
import           Data.Graph
import           Data.Graph.SCC
import           Data.List
import           Data.Map                                        (Map)
import qualified Data.Map                                        as Map
import           Data.Maybe
import           Data.Set                                        (Set)
import qualified Data.Set                                        as Set
import           Poets.Contracts.Language.CSL.AST
import           Poets.Contracts.Language.CSL.Exceptions
import           Poets.Contracts.Language.CSL.Render             ()
import           Poets.Contracts.Language.CSL.Typing.Match
import           Poets.Contracts.Language.CSL.Typing.TypeInferer hiding
                                                                  (RunTimeError)
import           Poets.Data                                      hiding
                                                                  (RunTimeError,
                                                                  Type)

-- |An atomic sub type constraint is simply represented by the two atomic types.
type AtomicSC = (AtomicType,AtomicType)

-- |A substitution on CSL type terms
type TypeSubst = Subst TypeSig TypeVarId

-- |A substitution on CSL atomic type terms
type AtomicTypeSubst = Subst AtomicTypeSig TypeVarId

-- |This function generates the nodes of a graph that represents the given
-- atomic subtyping constraints.
nodes :: [AtomicSC] -> [((), AtomicType,[AtomicType])]
nodes subs = map (\(t,ts) -> ((),t,ts)) $ Map.toList $ nodes' subs
    where nodes' :: [AtomicSC] -> Map AtomicType [AtomicType]
          nodes' scs = Map.unionWith (++)
                       (Map.fromListWith (++) (map (fmap (:[])) scs))
                       (Map.fromList (map (\(_,atp) -> (atp,[])) scs))

-- |This function takes a list of atomic subtyping constraints and removes
-- cycles from the constraints (i.e. something of the form @t1 <: t2: <: ... <:
-- tn <: t1@), by equating the types involved in a cycle. This is implemented by
-- generating a graph representing the sub typing relations constraints, and
-- then computing the SCCs of the graph.
combineSccs :: SrcPos -> [AtomicSC] -> Either CSLError ([AtomicSC], TypeSubst)
combineSccs pos scs = do
  let edges = sccGraph graph
  (_, scs', subst) <- traverse edges
  return (scs', subst)
    where (graph, f, _) = graphFromEdges $ nodes scs
          lookup v = let (_,k,_) = f v in k
          traverse :: [(SCC  Int, Int, [Int])]
                   -> Either CSLError (Map Int AtomicType,
                                       [AtomicSC],
                                       TypeSubst)
          traverse [] = return (Map.empty, [], Map.empty)
          traverse ((lscc,key,chs) : edges) = do
            (tys,scs,subst) <- traverse edges
            let lscc' = map lookup (flattenSCC lscc)
            (ty,subst') <- single pos lscc'
            let bind i = (ty, fromJust $ Map.lookup i tys)
            let scs' = map bind $ filter (/= key) chs
            return (Map.insert key ty tys, scs'++scs, subst' `Map.union` subst)

-- |This function takes a list of atomic types and tries to equate them. This
-- only works if there is at most one type constant among the atomic types. The
-- function returns the single atomic type, the input types are equated to, and
-- the corresponding atomic substitution that realizes the equating of the
-- types. If equating the types is not possible, i.e. there are at least two
-- type constants involved, a corresponding error is generated.
single :: SrcPos -> [AtomicType] -> Either CSLError (AtomicType,TypeSubst)
single _ [t] = return (t, Map.empty)
single pos ts =
    case cs of
      c1 : c2 : _ ->
          throwError $ TypeError ("Incompatible types '" ++ show c1 ++
                                  "' and '" ++ show c2 ++ "'") pos
      [c] ->
          let bind v = (v, undefined -- deepInject2 c
                       ) in
          return (c, Map.fromList $ map bind vs)
      [] ->
          let (var : vs') = vs in
          let bind v = (v, iTVar var :: Type) in
          return (iTVar var :: AtomicType, Map.fromList $ map bind vs')
    where (cs,vs) = partitionTypes ts
          partitionTypes [] = ([],[])
          partitionTypes (atp:atps) =
              let (cs,vs) = partitionTypes atps in
              case project atp of
                Just (TVar x) -> (cs, x:vs)
                _             -> (atp:cs, vs)


-- |Removes (some) redundant type constraints, such as replacing @a <: b <: c@
-- with @a <: b@, and mapping @c -> b@ in the substitution environment. The
-- first parameter is the atomic subtype constraints and the second parameter is
-- the /protected/ type variables, i.e., type variables that cannot be removed.
removeRedundancies :: [AtomicSC] -> Set TypeVarId -> ([AtomicSC], TypeSubst)
removeRedundancies subtypeConstraints protected =
    (mapBoth (snd3 . lookup) $ edges graph, Map.fromList substitution)
    where (substitution, graph) = fix reduce aboveGraph belowGraph []
          reduce aboveGraph belowGraph substitution =
              case [(a, b, a', b') | a <- vertices aboveGraph,
                                     b <- vertices aboveGraph,
                                     a /= b,
                                     (a', b') <- variables (a, b),
                                     not (a' `Set.member` protected),
                                     all ((\v -> v /= a' && v /= b') . fst) substitution,
                                     entails a b] of
                [] -> Nothing
                ((a, _b, a', b'):_) ->
                    Just ((removeVertex a aboveGraph,
                           removeVertex a belowGraph), (a', iTVar b' :: Type))
            where entails a b =
                      Set.delete a (above a) `Set.isSubsetOf` above b &&
                      Set.delete a (below a) `Set.isSubsetOf` below b
                  above = reach aboveGraph
                  below = reach belowGraph
          reach g s = Set.fromList $ s : reachable g s
          (belowGraph, lookup, _) = graphFromEdges $ nodes subtypeConstraints
          aboveGraph = transposeG belowGraph
          snd3 (_, v, _) = v
          mapBoth f = map (\(a, b) -> (f a, f b))
          variables (a, b) = [(a', b') | Just (TVar a') <- [project $ snd3 $ lookup a],
                                         Just (TVar b') <- [project $ snd3 $ lookup b]]
          fix f above below substitution =
              case f above below substitution of
                Just ((above, below), x) -> fix f above below (x : substitution)
                Nothing -> (reverse substitution, below)

-- |Remove a vertex from a graph by removing the vertex and connecting the
-- vertices from the ingoing edges to the vertices of the outgoing edges.
removeVertex :: Vertex -> Graph -> Graph
removeVertex vertex graph =
    buildG (Array.bounds graph) $ [(v, w) | (v, _) <- ingoing,
                                            (_, w) <- outgoing] ++ unrelated
    where ingoing = [(v, w) | (v, w) <- edges graph, w == vertex]
          outgoing = [(v, w) | (v, w) <- edges graph, v == vertex]
          unrelated = [(v, w) | (v, w) <- edges graph, w /= vertex, v /= vertex]

-- |Check if a single atomic sub type actually entails an equality, e.g.,
-- @a :<: Int@ entails that @a = Int@.
checkForEq :: (SubTypeConstraint :&: SrcPos) AtomicType -> Maybe AtomicTypeSubst
checkForEq (SubTypeConstraint tp1 tp2 :&: _) =
    case project tp1 of
      Just (TVar x) ->
          case project tp2 of
            -- If tp2 is an atomic type, then only if tp2 is a Real or a
            -- record type do we not necessarily have equality
            Just TReal       -> Nothing
            Just (TRecord _) -> Nothing
            Just _           -> Just $ Map.singleton x tp2
            Nothing          -> Nothing
      Nothing ->
          case project tp2 of
            Just (TVar x) ->
                -- If tp1 is an atomic type, then only if tp2 is a Real or a
                -- record type do we not necessarily have equality
                case project tp1 of
                  Just TInt        -> Nothing
                  Just (TRecord _) -> Nothing
                  Just _           -> Just $ Map.singleton x tp1
                  Nothing          -> Nothing
            Nothing ->
                Nothing

-- |Check if a set of atomic sub types contain actual equalities, e.g.,
-- @a :<: Int@ entails that @a = Int@.
checkForEqs :: [(SubTypeConstraint :&: SrcPos) AtomicType]
            -> AtomicTypeSubst
checkForEqs = foldl (\subst -> maybe subst (Map.union subst) . checkForEq)
                    Map.empty

-- |Check if a single atomic sub type constraint is consistent. An error is
-- thrown if the atomic sub type is inconsistent, otherwise a boolean value
-- indicating whether the constraint cannot be safely removed is returned.
checkSubC :: RecEnv
          -> (SubTypeConstraint :&: SrcPos) AtomicType
          -> Either CSLError Bool
checkSubC recordEnv (SubTypeConstraint tp1 tp2 :&: p) =
    if tp1 == tp2 then
        return False
    else
        case (project tp1, project tp2) of
          (Just TInt, Just TReal) -> return False
          (Just (TRecord r1), Just (TRecord r2)) ->
              case isSubType recordEnv r1 r2 of
                Left err ->
                    throwError $ RunTimeError err
                Right False ->
                    subTypeError tp1 tp2
                Right True ->
                    return False
          (Just _, Just _) -> subTypeError tp1 tp2
          (_, _) -> return True -- keep
    where subTypeError tp1 tp2 =
              throwError $ TypeError ("'" ++ show tp1 ++
                                      "' is not a sub type of '" ++
                                      show tp2 ++ "'") p

-- |Check if a single field type constraint is consistent. An error is
-- thrown if the constraint is inconsistent, otherwise a sub type constraint
-- is generated, which in turn means that all sub type constraints have to be
-- (once again) matched.
checkFieldCs :: Int
             -> RecEnv
             -> [(SubTypeConstraint :&: SrcPos) AtomicType]
             -> [(FieldConstraint :&: SrcPos) Type]
             -> Either CSLError (Int,
                                 [(SubTypeConstraint :&: SrcPos) AtomicType],
                                 [(FieldConstraint :&: SrcPos) Type],
                                 Subst TypeSig TypeVarId)
checkFieldCs iter _ subcs [] = return (iter, subcs, [], Map.empty)
checkFieldCs iter recordEnv subcs (fc : fcs) = do
    (iter', subcs', fcs', s) <- checkFieldCs iter recordEnv subcs fcs
    let FieldConstraint tp1 f tp2 :&: p = appSubst s fc
    case project tp1 of
      (Just (TRecord r1)) ->
          case getFields recordEnv r1 of
            Left _ ->
                fieldTypeError tp1 f tp2 p
            Right fEnv ->
                case fmap fieldType (getFieldInfo fEnv f) of
                  Left _ ->
                      fieldTypeError tp1 f tp2 p
                  Right tp -> do
                      -- Then perform matching
                      (asubcs,s) <- either (\(MatchError e p) -> throwError $ TypeError e p)
                                           return
                                           (match ((SubTypeConstraint tp tp2 :&: p) : map (fmap undefined -- deepInject2
                                                                                          ) subcs') iter')
                      return (iter' + 1, asubcs, map (appSubst s) fcs', s)
      (Just _) ->
          fieldTypeError tp1 f tp2 p
      Nothing ->
          -- Only if tp1 is a type variable will we not signal an error
          case project tp1 of
            (Just (TVar _)) ->
                return (iter', subcs', (FieldConstraint tp1 f tp2 :&: p) : fcs', s)
            Nothing ->
                fieldTypeError tp1 f tp2 p
    where fieldTypeError tp1 f tp2 p =
              throwError $ TypeError ("'" ++ show tp1 ++
                                      "' does not contain a field '" ++
                                      show f ++ "' of type '" ++
                                      show tp2 ++ "'") p

-- |Simplify a CSL typing, by (a) equating cyclic sub typings, and (b) removing
-- unneeded, intermediate sub typings.
simplify' :: Int
          -> RecEnv
          -> SrcPos
          -> Type
          -> [(SubTypeConstraint :&: SrcPos) AtomicType]
          -> [(FieldConstraint :&: SrcPos) Type]
          -> [(EqConstraint :&: SrcPos) Type]
          -> [(OrderConstraint :&: SrcPos) Type]
          -> Either CSLError (Int,
                              Type,
                              [(SubTypeConstraint :&: SrcPos) AtomicType],
                              [(FieldConstraint :&: SrcPos) Type],
                              [(EqConstraint :&: SrcPos) Type],
                              [(OrderConstraint :&: SrcPos) Type])
simplify' iter recordEnv pos tp scs fcs ecs ocs = do
  (scs1, subst1) <- combineSccs pos $ map unpack scs
  let tp1 = appSubst subst1 tp
  let fcs1 = map (fmap (appSubst subst1)) fcs
  let ecs1 = map (fmap (appSubst subst1)) ecs
  let ocs1 = map (fmap (appSubst subst1)) ocs
  let xs = variables tp1
  let fcxs = Set.unions $ map variables $ concatMap toList fcs1
  let ecxs = Set.unions $ map variables $ concatMap toList ecs1
  let ocxs = Set.unions $ map variables $ concatMap toList ocs1
  let protected = Set.unions [xs, fcxs, ecxs, ocxs]
  let (scs2, subst2) = removeRedundancies scs1 protected
  let tp2 = appSubst subst2 tp1
  let fcs2 = map (fmap (appSubst subst2)) fcs1
  let ecs2 = map (fmap (appSubst subst2)) ecs1
  let ocs2 = map (fmap (appSubst subst2)) ocs1
  -- We try to annotate sub typing constraints
  let scs3 = map (\(atp1,atp2) ->
                      case find (\c -> unpack c == (atp1,atp2)) scs of
                        Just c -> c
                        _      -> pack atp1 atp2 pos)
                 scs2
  let subst3 = checkForEqs scs3
  let subst4 = Map.map deepInject
               subst3 :: TypeSubst
  let scs4 = map (fmap (appSubst subst3)) scs3
  let tp4 = appSubst subst4 tp2
  let fcs4 = map (fmap (appSubst subst4)) fcs2
  let ecs4 = map (fmap (appSubst subst4)) ecs2
  let ocs4 = map (fmap (appSubst subst4)) ocs2
  scs5 <- filterM (checkSubC recordEnv) scs4
  (iter', scs6, fcs6, subst6) <- checkFieldCs iter recordEnv scs5 fcs4
  let tp6 = appSubst subst6 tp4
  let ecs6 = map (fmap (appSubst subst6)) ecs4
  let ocs6 = map (fmap (appSubst subst6)) ocs4
  return (iter', tp6, scs6, fcs6, ecs6, ocs6)
      where unpack (SubTypeConstraint atp1 atp2 :&: _) = (atp1, atp2)
            pack atp1 atp2 p = SubTypeConstraint atp1 atp2 :&: p

-- We need the following instance in order to test type constraints for equality
instance (EqF c, EqF t) => Eq ((c :&: SrcPos) (Term t)) where
    (a :&: _) == (b :&: _) = a `eqF` b

-- |Simplify a CSL typing, by (a) equating cyclic sub typings, and (b) removing
-- unneeded, intermediate sub typings. Executes simplifications repeatedly
-- until a fixed-point is reached.
simplify :: Int
         -> RecEnv
         -> SrcPos
         -> Type
         -> [(SubTypeConstraint :&: SrcPos) AtomicType]
         -> [(FieldConstraint :&: SrcPos) Type]
         -> [(EqConstraint :&: SrcPos) Type]
         -> [(OrderConstraint :&: SrcPos) Type]
         -> Either CSLError (Type,
                             [(SubTypeConstraint :&: SrcPos) AtomicType],
                             [(FieldConstraint :&: SrcPos) Type],
                             [(EqConstraint :&: SrcPos) Type],
                             [(OrderConstraint :&: SrcPos) Type])
simplify iter recordEnv pos tp subcs fcs ecs ocs = do
  (iter', tp1, subcs1, fcs1, ecs1, ocs1) <- simplify' iter recordEnv pos tp subcs fcs ecs ocs
  if (tp1, subcs1, fcs1, ecs1, ocs1) == (tp, subcs, fcs, ecs, ocs) then
      return (tp1, subcs1, fcs1, ecs1, ocs1)
  else
      simplify iter' recordEnv pos tp1 subcs1 fcs1 ecs1 ocs1
