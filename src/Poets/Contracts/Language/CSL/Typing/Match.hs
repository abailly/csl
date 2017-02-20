{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Poets.Contracts.Language.CSL.Typing.Match
-- Copyright   : 3gERP, 2010
-- License     : All Rights Reserved
--
-- Maintainer  : Tom Hvitved
-- Stability   : unknown
-- Portability : unknown
--
-- This module implements a match algorithm according to Fuh and
-- Mishra [Theor. Comp. Sci. 73 (1990) 155-175]. (Implementation adopted from
-- Patrick Bahr's implementation in
-- 'Poets.Reporting.Language.Parrot.Typing.Match'.)
--
--------------------------------------------------------------------------------

module Poets.Contracts.Language.CSL.Typing.Match
    (
     AtomicType,
     AtomicTypeSig,
     MError(..),
     match
    ) where

import           Control.Monad                                   hiding (mapM,
                                                                  sequence)
import           Control.Monad.Error
import           Control.Monad.Reader                            hiding (mapM,
                                                                  sequence)
import           Control.Monad.State                             hiding (mapM,
                                                                  sequence)
import           Data.Comp.Decompose
import           Data.Comp.Derive
import           Data.Comp.Ops
import           Data.Comp.Variables
import           Data.Equivalence.Monad
import qualified Data.Map                                        as Map
import           Data.Set                                        (Set)
import qualified Data.Set                                        as Set
import           Poets.Contracts.Language.CSL.AST
import           Poets.Contracts.Language.CSL.Render             ()
import           Poets.Contracts.Language.CSL.Typing.TypeInferer
import           Poets.Data                                      hiding (Type)
import           Prelude                                         hiding (mapM,
                                                                  sequence)

-- |Atomic types are either type constants or type variables.
type AtomicType = Term AtomicTypeSig

-- |Atomic types are either type constants or type variables.
type AtomicTypeSig = TypeConstant :+: TypeVar

-- |A sub type constraint including source position information.
type SubTypeConstraintPos = SubTypeConstraint :&: SrcPos

-- |This is the type of the state of the matching monad 'MatchM'. It contains
-- the accumulated substitution and a set of sub typing constraints that
-- still need to be processed. Furthermore, it also contains a counter for
-- generating fresh type variables.
data MatchState = MatchState{
      msSubst            :: Subst TypeSig TypeVarId,
      msConstraints      :: [SubTypeConstraintPos Type],
      msVarCounter       :: Int,
      msVarGlobalCounter :: Int
    }

-- |Matching errors.
data MError = MatchError String SrcPos

instance Error MError where
    strMsg e = MatchError e Nothing

-- |The match monad.
type MatchM a = forall s. EquivT s (Set AtomicType) AtomicType
                (ReaderT SrcPos (StateT MatchState (Either MError))) a

-- |This function runs the 'MatchM' monad. It takes a list of sub typing
-- constraints, and returns the generated substitution.
runMatchM :: [SubTypeConstraintPos Type]
          -> Int
          -> MatchM ()
          -> Either MError (Subst TypeSig TypeVarId)
runMatchM cs counter k =
    liftM arr $
    (`runStateT` state) $
    (`runReaderT` Nothing) $
    runEquivT Set.singleton Set.union k
    where
      arr ((),MatchState{msSubst = subst}) = subst
      state = MatchState{msSubst = Map.empty,
                          msConstraints = cs,
                          msVarCounter = 0,
                          msVarGlobalCounter = counter}

{-| This function runs the given 'MatchM' monad in the context of the
given position. This is meant to be the position information of a
sub typing constraint which is then used when constructing the atomic
sub typing constraints obtained from it. -}
withConstrPos :: SrcPos -> MatchM a -> MatchM a
withConstrPos pos m = local (\oldPos -> pos `mplus` oldPos) m

-- |Get the current source position.
getConstrPos :: MatchM SrcPos
getConstrPos = ask

-- |Generate a fresh type variable identifier.
getFreshTypeVar :: MatchM TypeVarId
getFreshTypeVar = do
  state <- get
  put $ state{msVarCounter = msVarCounter state + 1}
  return $ "_match_" ++ show (msVarGlobalCounter state) ++ "_" ++
           show (msVarCounter state)

-- |Get the current set of sub type constraints.
getConstraints :: MatchM [SubTypeConstraintPos Type]
getConstraints = liftM msConstraints get

-- |Set the current set of sub type constraint.
putConstraints :: [SubTypeConstraintPos Type] -> MatchM ()
putConstraints cs = do
  st <- get
  put st {msConstraints = cs}

-- |Get the generated substitution.
getSubst :: MatchM (Subst TypeSig TypeVarId)
getSubst = liftM msSubst get

-- |Set the generated substitution.
putSubst :: Subst TypeSig TypeVarId -> MatchM ()
putSubst ws = do
  st <- get
  put st {msSubst = ws}

-- |This function takes a 'MatchM' monad depending on a sub typing constraint
-- and runs it with the next sub typing constraint in the set of constraints,
-- and afterwards runs the 'MatchM' monad given as the second argument. If the
-- set is empty, this function does nothing.
nextConstr :: (SubTypeConstraint Type -> MatchM ()) -> MatchM () -> MatchM ()
nextConstr f cont = do
  cs <- getConstraints
  case cs of
    [] ->
        return ()
    (c :&: pos):cs' ->
        putConstraints cs' >>
        withConstrPos pos (f c) >>
        cont

-- |This function adds the given list of sub typing constraints to the state. A
-- sub type constraint is represented by its variance, and the two types.
addSubTypeConstraints :: [(Variance, Type, Type)] -> MatchM ()
addSubTypeConstraints cs = do
  cs' <- getConstraints
  pos <- getConstrPos
  let cs'' = map (\c -> subTypeConstraint c :&: pos) cs
  putConstraints $ cs'' ++ cs'

-- |Generate a sub type constraint from a variant and the two types.
subTypeConstraint :: (Variance, t, t) -> SubTypeConstraint t
subTypeConstraint (Covariant,s,t)     = SubTypeConstraint s t
subTypeConstraint (Contravariant,s,t) = SubTypeConstraint t s

-- |Given a type variable identifier, return the set of equivalent type
-- variable identifiers, and type constants.
getEquivs :: TypeVarId -> MatchM ([TypeVarId],[Term TypeConstant])
getEquivs x = do
  atps <- classDesc (iTVar x)
  return $ part $ Set.toList atps
      where  part [] = ([],[])
             part (atp:atps) = let (xs,ctps) = part atps in
                               case project atp of
                                 Just (TVar x) -> (x:xs,ctps)
                                 _ -> let Just ctp = deepProject atp in
                                      (xs,ctp:ctps)


--------------------------------------------------------------------------------
-- Variance definitions
--------------------------------------------------------------------------------

-- |The variance of a type parameter.
data Variance = Contravariant | Covariant

-- |Type class for defining the variances of a type signature. E.g., the list
-- type constructor is @Covariant@ in its only type argument, whereas the
-- function type constructor is @Contravariant@ in its first argument and
-- @Covariant@ in its second.
class Variant f where
    variances :: Const f -> [Variance]

$(derive [liftSum] [''Variant])

instance Variant TypeConstant where
    variances _ = []

instance Variant TypeList where
    variances _ = [Covariant]

instance Variant TypeEnt where
    variances _ = [Covariant]

instance Variant TypeUnit where
    variances _ = []

instance Variant TypeFunction where
    variances _ = [Contravariant,Covariant]

instance Variant TypeVar where
    variances _ = []


--------------------------------------------------------------------------------
-- 'ALLNEW' (p. 168)
--------------------------------------------------------------------------------

-- |The @AllNew@ algebra replaces all atomic types or type variables in a type
-- expression with fresh type variables. Furthermore, the replaced type
-- expressions are equated with the newly generated type variables.
class (TypeVar :<: g) => AllNew f g where
    allNewAlg :: f (Term g) -> MatchM (Term g)

instance (AllNew f h, AllNew g h) => AllNew (f :+: g) h where
    allNewAlg (Inl x) = allNewAlg x
    allNewAlg (Inr x) = allNewAlg x

instance (Functor g, TypeVar :<: g) => AllNew TypeConstant g where
    allNewAlg TInt = do
      x <- getFreshTypeVar
      equate iTInt (iTVar x)
      return $ iTVar x
    allNewAlg TBool = do
      x <- getFreshTypeVar
      equate iTBool (iTVar x)
      return $ iTVar x
    allNewAlg TString = do
      x <- getFreshTypeVar
      equate iTString (iTVar x)
      return $ iTVar x
    allNewAlg TDate = do
      x <- getFreshTypeVar
      equate iTDate (iTVar x)
      return $ iTVar x
    allNewAlg TTime = do
      x <- getFreshTypeVar
      equate iTTime (iTVar x)
      return $ iTVar x
    allNewAlg TDateTime = do
      x <- getFreshTypeVar
      equate iTDateTime (iTVar x)
      return $ iTVar x
    allNewAlg TDuration = do
      x <- getFreshTypeVar
      equate iTDuration (iTVar x)
      return $ iTVar x
    allNewAlg TReal = do
      x <- getFreshTypeVar
      equate iTReal (iTVar x)
      return $ iTVar x
    allNewAlg (TRecord rName) = do
      x <- getFreshTypeVar
      equate (iTRecord rName) (iTVar x)
      return $ iTVar x

instance (TypeList :<: g, TypeVar :<: g) => AllNew TypeList g where
    allNewAlg (TList tp) = return $ iTList tp

instance (TypeEnt :<: g, TypeVar :<: g) => AllNew TypeEnt g where
    allNewAlg (TEnt ty) = return $ iTEnt ty

instance (TypeUnit :<: g, TypeVar :<: g) => AllNew TypeUnit g where
    allNewAlg TUnit = return iTUnit

instance (Functor g, TypeVar :<: g) => AllNew TypeVar g where
    allNewAlg (TVar x) = do
      y <- getFreshTypeVar
      equate (iTVar x) (iTVar y)
      return $ iTVar y

instance (TypeFunction :<: g, TypeVar :<: g) => AllNew TypeFunction g where
    allNewAlg (TFunction tp1 tp2) = return $ iTFunction tp1 tp2

-- |Replace all atomic type expressions and type variables with a fresh type
-- variable, and equate the newly created type variables with the original
-- type expressions.
allNewTerm :: Type -> MatchM Type
allNewTerm = cataM allNewAlg


--------------------------------------------------------------------------------
-- Matching algorithm
--------------------------------------------------------------------------------

instance HasVars TypeVar TypeVarId where
    isVar (TVar x) = Just x

instance HasVars TypeConstant TypeVarId where
    isVar _ = Nothing

instance HasVars TypeList TypeVarId where
    isVar _ = Nothing

instance HasVars TypeEnt TypeVarId where
    isVar _ = Nothing

instance HasVars TypeUnit TypeVarId where
    isVar _ = Nothing

instance HasVars TypeFunction TypeVarId where
    isVar _ = Nothing

-- |This is the implementation of the match algorithm within the 'MatchM' monad.
matchM :: MatchM ()
matchM = nextConstr matchM' matchM

-- |Perform matching on a set of sub type constraints. Either an error is thrown
-- (if the sub type constraints are inconsistent), or the resulting substitution
-- is returned. Furthermore, the substitution is applied to the original set of
-- sub type constraints, and simplified to atomic sub type constraints.
match :: [SubTypeConstraintPos Type]
      -> Int
      -> Either MError ([SubTypeConstraintPos AtomicType],
                        Subst TypeSig TypeVarId)
match cs counter = do
  s <- runMatchM cs counter matchM
  let cs' = map (fmap (appSubst s)) cs
  cs'' <- simplifyM cs'
  return (cs'',s)


coerceToAtomic :: Type -> Maybe AtomicType
coerceToAtomic = deepProject

-- |This function executes a the match algorithm on a single sub typing
-- constraint.
matchM' :: SubTypeConstraint Type -> MatchM ()
matchM' (SubTypeConstraint tp1 tp2) =
    case (coerceToAtomic tp1, coerceToAtomic tp2) of
      (Just atp1, Just atp2) ->
          -- tp1 and tp2 are atomic types, so perform atomic elimination
          -- (Definition 6.2)
          equate atp1 atp2
      _ ->
          case (decompose tp1, decompose tp2) of
            (Var x, _) ->
                -- tp1 is a type variable, so perform expansion
                expansion Covariant x tp2
            (_ , Var x) ->
                -- tp2 is a type variable, so perform expansion
                expansion Contravariant x tp1
            (Fun s1 args1, Fun s2 args2) ->
                -- Both tp1 and tp2 are compound types, so perform decomposition
                decomposition (s1,args1) (s2,args2)
    where -- Decomposition (Definition 6.1)
          decomposition :: (Const TypeSig, [Type])
                        -> (Const TypeSig, [Type]) -> MatchM ()
          decomposition (g1, tps1) (g2, tps2) = do
            when (g1 /= g2)
                 (subTypeError Covariant tp1 tp2
                               "; the types are structurally distinct")
            addSubTypeConstraints $ zip3 (variances g1) tps1 tps2
          -- Expansion (Definition 6.3)
          expansion variance x tp = do
            (xs,ctps) <- getEquivs x
            if not $ null ctps then
                subTypeError variance (deepInject $ head ctps) tp
                             "; the types are structurally distinct"
            else do
              astps <- atomicSubs tp
              atps <- filterM (equivalent (iTVar x)) astps
              if not $ null atps then
                  subTypeError variance (deepInject $ head atps) tp
                                 "; cannot construct infinite type"
              else do mapM_ expand xs
                      removeClass (iTVar x)
                      return ()
              where expand x = do
                      tp' <- allNewTerm tp
                      let subst = Map.singleton x tp'
                      s <- getSubst
                      putSubst $ Map.union s subst
                      cs <- getConstraints
                      putConstraints $ map (fmap (appSubst subst)) cs
          -- Generate a list of all atomic sub terms from a given type term
          atomicSubs :: Type -> MatchM [AtomicType]
          atomicSubs tp =
              case deepProject tp of
                Just atp ->
                    -- The type is it self atomic, so return it
                    return [atp]
                _ ->
                    -- The type is not atomic, so inspect all sub terms
                    case decompose tp :: Decomp TypeSig TypeVarId Type of
                      Fun _ args -> liftM concat $ mapM atomicSubs args
                      _ -> fail "Error in atomicSubs: expected compound type"

-- |Simplify a set of (already 'match'ed) sub type constraints into a set of
-- atomic sub type constraints.
simplifyM :: Monad m => [SubTypeConstraintPos Type]
          -> m [SubTypeConstraintPos AtomicType]
simplifyM [] = return []
simplifyM ((SubTypeConstraint tp1 tp2 :&: pos) : cs) =
    case (deepProject tp1, deepProject tp2) of
      (Just atp1, Just atp2) -> do
        acs <- simplifyM cs
        return $ (SubTypeConstraint atp1 atp2 :&: pos) : acs
      _ ->
          case (decompose tp1 :: Decomp TypeSig TypeVarId Type,
                decompose tp2 :: Decomp TypeSig TypeVarId Type) of
            (Fun s1 args1, Fun s2 args2) -> do
              when (s1 /= s2)
                   (fail $ "Error in simplifyM: " ++
                           "mismatch between type constructors")
              simplifyM (map (\x -> subTypeConstraint x :&: pos)
                             (zip3 (variances s1) args1 args2) ++ cs)
            _ -> fail "Error in simplifyM: expected compound types"

{-| This function reports an error that states that the given
sub typing constraint cannot be satisfied. The sub typing constraint @t1
< t2@ if the variance argument is 'Covariant' and vice versa
otherwise, where @t1@, @t2@ are the first and the second type argument
to this function. -}
subTypeError :: Variance -> Type -> Type -> String -> MatchM ()
subTypeError v t1 t2 msg = do
  pos <- getConstrPos
  throwError $ MatchError ("'" ++ show s1 ++ "' cannot be a subtype of '" ++
                           show s2 ++ "'" ++ msg) pos
    where (s1,s2) = case v of
                      Covariant     -> (t1,t2)
                      Contravariant -> (t2,t1)
