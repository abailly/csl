{-# LANGUAGE ScopedTypeVariables, TypeOperators, FlexibleInstances,
  MultiParamTypeClasses, FlexibleContexts, TypeSynonymInstances,
  UndecidableInstances, TemplateHaskell, DoAndIfThenElse #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts.Language.CSL.Typing.TypeInferer
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines type inference for CSL expressions.
--
--------------------------------------------------------------------------------
module Poets.Contracts.Language.CSL.Typing.TypeInferer
    (
     TypeInfError(..),
     SubTypeConstraint(..),
     FieldConstraint(..),
     EqConstraint(..),
     OrderConstraint(..),
     TrackingConstraint(..),
     TypeVarId,
     TypeInferer(..),
     TypeEnv(..),
     Typing(..),
     typeInfTerm,
     simpleTyping
    ) where

import Prelude hiding (EQ, LT, GT, mapM_, concatMap, mapM, sequence, showList)
import Control.Monad.Error hiding (mapM_, mapM, sequence)
import Control.Monad.Reader hiding (mapM_, mapM, sequence)
import Control.Monad.State hiding (mapM_, mapM, sequence)
import Data.List (intercalate, sort, (\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Comp
import Data.Comp.Derive
import Data.Comp.Decompose
import Data.Comp.Variables
import qualified Data.Comp.Unification as U
import Data.Foldable hiding (foldl, concat)
import Data.Traversable
import Poets.Data.Value
import Poets.Data.Render
import Poets.Data.Type
import Poets.Data.Type.Utils
import Poets.Data.Value.Utils
import Poets.Contracts.Language.CSL.AST (SrcPos)
import Poets.Contracts.Language.CSL.AST.Exp
import Poets.Contracts.Language.CSL.AST.Type

-- |Potential errors during type inference.
data TypeInfError t =
    -- |Unification error: infinite type.
    FailedOccursCheck TypeVarId (Term t)
    -- |Unification error: inferred type different from expected type.
  | HeadSymbolMismatch (Term t) (Term t)
    -- |Runtime error (unexpected error).
  | RunTimeError String
    -- |Type error: Undefined record type.
  | RecordUndefined RecordName
    -- |Type error: Abstract record type.
  | RecordAbstract RecordName
    -- |Type error: Record field(s) are missing.
  | FieldsMissing RecordName [FieldName]
    -- |Type error: Undefined record field(s).
  | FieldsUndefined RecordName [FieldName]
    -- |Type error: unbound variable.
  | FreeVarError Var
    -- |Type error: no common super type in case expression.
  | CaseCommonSuperTypeError String
    -- |Type error: non-exhaustive patterns.
  | CaseNoneExhaustive [RecordName]

instance Error (TypeInfError t, SrcPos) where
    strMsg s = (RunTimeError s, Nothing)

instance (Functor t, Render t) => Show (TypeInfError t) where
    show (FailedOccursCheck v tp) =
        "Cannot construct infinite type '" ++ v ++ " = " ++ show tp ++ "'"
    show (HeadSymbolMismatch tp1 tp2) =
        "Type mismatch: inferred type '" ++ show tp1 ++
        "', expected type '" ++ show tp2 ++ "'"
    show (RunTimeError e) =
        "Unexpected runtime error:" ++ e
    show (RecordUndefined rName) =
        "Record type '" ++ rName ++ "' is undefined"
    show (RecordAbstract rName) =
        "Record type '" ++ rName ++ "' is abstract"
    show (FieldsMissing rName fNames) =
        "The field(s) " ++ showList fNames ++
        " are missing for record type '" ++ rName ++ "'"
    show (FieldsUndefined rName fNames) =
        "The field(s) " ++ showList fNames ++
        " are undefined for record type '" ++ rName ++ "'"
    show (FreeVarError x) =
        "Not in scope: '" ++ x ++ "'"
    show (CaseCommonSuperTypeError e) =
        "Unable to determine common super type in case expression: " ++ e
    show (CaseNoneExhaustive rNames) =
        "Non-exhaustive pattern match(es). Patterns not matched: " ++
        intercalate ", " rNames

showList :: [String] -> String
showList = intercalate ", " . map (\x -> "'" ++ x ++ "'")

-- |The (internal) type of type variables used in unification.
type TypeVarId = String

-- |A type constraint over constraint signature @c@, annotated with source
-- position information, and constraining types over signature @t@.
type TypeConstraint c t = (c :&: SrcPos) (Term t)

-- |A /set/ of type constraints.
type TypeConstraints c t = Set (TypeConstraint c t)

-- |Sub type constraint. @SubTypeConstraint tp1 tp2@ means that @tp1@ is
-- required to be a sub type of @tp2@, i.e., @tp1 <: tp2@.
data SubTypeConstraint tp = SubTypeConstraint tp tp

instance Show tp => Show (SubTypeConstraint tp) where
    show (SubTypeConstraint tp1 tp2) = show tp1 ++ " <: " ++ show tp2

-- |Field type constraint. @FieldConstraint tp1 f tp2@ means that @tp1@ must
-- be a record type, which contains a field @f@ of type @tp2@.
data FieldConstraint tp = FieldConstraint tp FieldName tp

instance Show t => Show (FieldConstraint t) where
    show (FieldConstraint tp1 f tp2) = show tp1 ++ "." ++ f ++ ": " ++ show tp2

-- |Equality type constraint. @EqConstraint tp@ means that @tp@ must be an
-- equality type, e.g., @String@.
data EqConstraint tp = EqConstraint tp

instance Show t => Show (EqConstraint t) where
    show (EqConstraint tp) = "Eq(" ++ show tp ++ ")"

-- |Order type constraint. @OrderConstraint tp@ means that @tp@ must be an
-- ordered type, e.g., @Int@.
data OrderConstraint tp = OrderConstraint tp

instance Show t => Show (OrderConstraint t) where
    show (OrderConstraint tp) = "Ord(" ++ show tp ++ ")"

-- |Auxiliary type constraint used to track potential changes made to newly
-- introduced type variables during unification. Example: in the expression,
-- @\\x -> if x then 1 else 2@", @x@ will initially be assigned a fresh type,
-- @a@, but unification will later force @a@ to type @Bool@. This will be
-- recorded by the \"type constraint\" @TrackingConstraint a TBool@, so we can
-- -- after doing type inference of the body -- assign the function the type
-- @Bool -> Int@ (rather than @a -> Int@).
data TrackingConstraint e = TrackingConstraint TypeVarId e

instance Show t => Show (TrackingConstraint t) where
    show (TrackingConstraint x tp) = "<tracked " ++ x ++ " = " ++ show tp ++ ">"

$(derive [makeFunctor, makeFoldable, makeEqF, makeOrdF]
         [''SubTypeConstraint, ''FieldConstraint, ''EqConstraint,
          ''OrderConstraint, ''TrackingConstraint])

-- |Type class for generating (fresh) type variables from integers.
class GenVar t where
    genVar :: Int -> Term t

instance (TypeVar :<: t) => GenVar t where
    genVar n = iTVar $ "__" ++ show n

-- |The state used in type inference includes a counter for generating fresh
-- type variable names, and a set of current type constraints. For generating
-- better error messages, each type constraint is annotated with a subset of
-- the environment.
data TypeInfState c t = TypeInfState{
      typeInfStateVarCounter :: Int,
      typeInfStateTypeConstraints :: TypeConstraints c t
    }

-- |A variable (typing) environment is a mapping of variables to their typing.
type VariableEnv c t = Map Var (Typing c t)

-- |The typing environment used during type inference.
data TypeEnv c t = TypeEnv{
      recordTypingEnv :: RecordEnv (Term t),
      entityTypingEnv :: EntityTypingEnv,
      variableEnv :: VariableEnv c t,
      srcPos :: SrcPos
    }

-- |The type inference monad.
type TI c t = ReaderT (TypeEnv c t) (ErrorT (TypeInfError t, SrcPos)
                                            (State (TypeInfState c t)))

-- |A typing consists of an inferred type, an indication whether the type is
-- polymorphic or not, and a set of type constraints. Each type constraint is
-- annotated (e.g. with source code information). 
data Typing c t = Typing{
      typingType :: Term t,
      typingPoly :: Bool,
      typingTypeConstraints :: TypeConstraints c t
    }

-- |Construct a /simple/ typing, i.e., a typing with no type constraints, and
-- which is /not/ polymorphic.
simpleTyping :: Term t -> Typing c t
simpleTyping tp = Typing{typingType = tp,
                         typingPoly = False,
                         typingTypeConstraints = Set.empty}

-- |Throw a type error. Besides throwing the passed value, the error also
-- includes a subset of the environment for debugging purposes (e.g., source
-- position information).
typeInfError :: (MonadReader (TypeEnv c t) m,
                 MonadError (TypeInfError t, SrcPos) m) =>
                TypeInfError t -> m b
typeInfError e = do
  a <- asks srcPos
  throwError (e, a)

-- |Throw a run-time error.
typeInfRunTimeError :: (MonadReader (TypeEnv c t) m,
                        MonadError (TypeInfError t, SrcPos) m) => String -> m b
typeInfRunTimeError e = typeInfError $ RunTimeError e

-- |Generate a fresh type variable.
getFreshTypeVar :: (MonadState (TypeInfState c t) m, GenVar t) => m (Term t)
getFreshTypeVar = do
  state <- get
  let i = typeInfStateVarCounter state
  put $ state{typeInfStateVarCounter = i + 1}
  return $ genVar i

-- |Retrieve the current set of type constraints.
getConstraints :: MonadState (TypeInfState c t) m => m (TypeConstraints c t)
getConstraints = liftM typeInfStateTypeConstraints get

-- |Set the current set of type constraints.
setConstraints :: MonadState (TypeInfState c t) m => TypeConstraints c t
               -> m ()
setConstraints cs = do
  state <- get
  put state{typeInfStateTypeConstraints = cs}

-- |Add a type constraint. The type constraint is automatically annotated with
-- information from the typing environment.
addConstraint :: (Ord (TypeConstraint c t),
                  MonadReader (TypeEnv c t) m,
                  MonadState (TypeInfState c t) m,
                  f :<: c) => f (Term t) -> m ()
addConstraint c = do
  a <- asks srcPos
  addConstraint' $ inj c :&: a

-- |Add an (already annotated) type constraint.
addConstraint' :: (Ord (TypeConstraint c t),
                   MonadReader (TypeEnv c t) m,
                   MonadState (TypeInfState c t) m) =>
                  (c :&: SrcPos) (Term t) -> m ()
addConstraint' c = do
  cs <- getConstraints
  setConstraints $ Set.insert c cs

-- |Add a sub type constraint. The sub type constraint is automatically
-- annotated with information from the typing environment.
addSubTypeConstraint :: (Ord (TypeConstraint c t),
                         SubTypeConstraint :<: c,
                         MonadReader (TypeEnv c t) m,
                         MonadState (TypeInfState c t) m) =>
                        Term t -> Term t -> m ()
addSubTypeConstraint tp1 tp2 = addConstraint $ SubTypeConstraint tp1 tp2

-- |Add a field constraint.
addFieldConstraint tp1 f tp2 = addConstraint $ FieldConstraint tp1 f tp2

-- |Add an equality type constraint.
addEqConstraint tp = addConstraint $ EqConstraint tp

-- |Add an order type constraint.
addOrderConstraint tp = addConstraint $ OrderConstraint tp

-- |Add a tracking constraint.
addTrackingConstraint x = addConstraint $ TrackingConstraint x (iTVar x)

-- |Track the type assigned to a type variable during unification.
trackTypeVar x = do
  cs <- getConstraints
  let Just (c :&: _) = find (\(c :&: _) ->
                                 case proj c of
                                   Just (TrackingConstraint y _) -> x == y
                                   _ -> False) cs
  let Just (TrackingConstraint _ tp) = proj c
  return tp

-- |Unify two types. Either an error is thrown, or a substitution is returned,
-- which is automatically applied to all type constraints.
unify :: (Functor c,
          Ord (TypeConstraint c t),
          MonadState (TypeInfState c t) m,
          Eq (Const t),
          Traversable t,
          Render t,
          HasVars t TypeVarId,
          MonadError (TypeInfError t, SrcPos) m,
          MonadReader (TypeEnv c t) m) => U.Equations t -> m (Subst t TypeVarId)
unify eqs = do 
  subst <- liftUnifErr $ U.unify eqs
  cs <- getConstraints
  setConstraints $ Set.map (fmap (appSubst subst)) cs
  return subst
      where liftUnifErr (Right res) =
                return res
            liftUnifErr (Left err) = 
                case err of 
                  U.FailedOccursCheck v t
                      -> typeInfError $ FailedOccursCheck v t
                  U.HeadSymbolMismatch t1 t2
                      -> typeInfError $ HeadSymbolMismatch t1 t2
                  U.UnifError msg
                      -> typeInfError $ RunTimeError $
                         "Internal error while performing unification:\n" ++ msg

-- |The type inference algebra. An instance, 'TypeInferer' @f@ @c@
-- @t@ @a@, means that terms over the signature @f@ can be type inferred,
-- generating a set of constraints over the signature
-- @c@, where the type terms are over the signature @t@.
class (Ord (TypeConstraint c t),
       Functor c,
       Foldable t,
       Render t,
       HasVars t TypeVarId,
       GenVar t,
       SubTypeConstraint :<: c) => TypeInferer f c t where
    typeInfAlg :: Alg f (TI c t (Term t))

$(derive [liftSum] [''TypeInferer])

-- |Infer the type (i.e., typing) of a term.
typeInfTerm :: (Functor f, TypeInferer f c t) =>
               RecordEnv (Term t)
            -> EntityTypingEnv
            -> VariableEnv c t
            -> Term f -- ^The term to infer a typing for.
            -> Either (TypeInfError t, SrcPos) (Typing c t) -- ^The result is either an (annotated) error, or the inferred type, and a set of (annotated) type constraints, i.e., a 'Typing'.
typeInfTerm recordTypingEnv entityTypingEnv variableEnv e = do
  let env = TypeEnv recordTypingEnv entityTypingEnv variableEnv Nothing
  let res = runState (runErrorT $ runReaderT (cata typeInfAlg e) env)
                     TypeInfState{typeInfStateVarCounter = 0,
                                  typeInfStateTypeConstraints = Set.empty}
  tp <- fst res
  return Typing{typingType = tp,
                typingPoly = True, -- (Top-level) typings are always polymorphic
                typingTypeConstraints = typeInfStateTypeConstraints $ snd res}

instance (Ord (TypeConstraint c t),
          Foldable t,
          Functor c,
          TypeConstant :<: t,
          TypeList :<: t,
          TypeEnt :<: t,
          Render t,
          HasVars t TypeVarId,
          GenVar t,
          EqF t,
          Eq (Const t),
          SubTypeConstraint :<: c) => TypeInferer Val c t where
    typeInfAlg (VInt _) =
        return iTInt
    typeInfAlg (VBool _) =
        return iTBool
    typeInfAlg (VString _) =
        return iTString
    typeInfAlg (VDate _) =
        return iTDate
    typeInfAlg (VTime _) =
        return iTTime
    typeInfAlg (VDateTime _) =
        return iTDateTime
    typeInfAlg VDuration{} =
        return iTDuration
    typeInfAlg (VReal _) =
        return iTReal
    typeInfAlg (VRecord vr) = do
      recordEnv <- asks recordTypingEnv
      let rName = vrecordName vr
      -- First check that the record type exists
      recInfo <- either (\_ -> typeInfError $ RecordUndefined rName)
                        return
                        (getFullRecordInfo recordEnv rName)
      -- Then check that the record type is not abstract
      when (isAbstract recInfo) (typeInfError $ RecordAbstract rName)
      -- Find record definition in typing environment
      let Right fields = getTypeFields recordEnv rName
      let fmxs = fieldsMap' $ vrecordFields vr
      let fieldNamesFound = sort $ Map.keys fmxs
      let fieldNamesExpected = fieldNamesSorted fields
      -- Report error if there is a mismatch between the expected fields and the
      -- fields found in the record value
      when (fieldNamesFound /= fieldNamesExpected)
           (let fieldNamesMissing = fieldNamesExpected \\ fieldNamesFound
                fieldNamesWrong = fieldNamesFound \\ fieldNamesExpected in
            if not $ null fieldNamesMissing then
                typeInfError $ FieldsMissing rName fieldNamesMissing
            else
                typeInfError $ FieldsUndefined rName fieldNamesWrong)
      -- Finally process each field, by adding a type constraint that the
      -- inferred type of a field is a sub type of the expected type
      mapM_ (processField fields) $ Map.toList fmxs
      -- Return record name as type
      return $ iTRecord rName
          where processField fields (f,mtp) = do
                   tp <- mtp
                   let Right field = getFieldInfo fields f
                   let tp' = {-deepInject3 $ -} fieldType field
                   addSubTypeConstraint tp tp'
    typeInfAlg (VEnt VEntity{ventType = rName, ventId = id}) = do
      recordEnv <- asks recordTypingEnv
      entityEnv <- asks entityTypingEnv
      -- Check that the entity typing is right
      either typeInfRunTimeError return $ checkEntityTyping recordEnv entityEnv id rName
      return $ iTEnt $ iTRecord rName
    typeInfAlg (VList mtps) = do
      -- A list value is well-typed, if there is a common super type for the
      -- elements of the list
      x <- getFreshTypeVar
      tps <- sequence mtps
      mapM_ (`addSubTypeConstraint` x) tps
      return $ iTList x

instance (TypeUnit :<: t,
          Ord (TypeConstraint c t),
          Foldable t,
          Functor c,
          Foldable c,
          Render t,
          HasVars t TypeVarId,
          GenVar t,
          SubTypeConstraint :<: c) => TypeInferer VUnit c t where
    typeInfAlg VUnit = return iTUnit

instance (Ord (TypeConstraint c t),
          Traversable t,
          Functor c,
          Foldable c,
          TypeConstant :<: t,
          TypeList :<: t,
          TypeVar :<: t,
          TypeFunction :<: t,
          Render t,
          HasVars t TypeVarId,
          Eq (Const t),
          EqF t,
          SubTypeConstraint :<: c,
          FieldConstraint :<: c,
          EqConstraint :<: c,
          OrderConstraint :<: c,
          TrackingConstraint :<: c) => TypeInferer CoreExp c t where
    typeInfAlg EVar{expVar = x} = do
      -- Lookup variable in the typing environment
      varEnv <- asks variableEnv
      tp <- maybe (typeInfError $ FreeVarError x) return (Map.lookup x varEnv)
      -- Variable is in scope, so we must first check whether the variable has a
      -- polymorphic type
      if not $ typingPoly tp then
          -- Type is not polymorphic, so return it right away
          do mapM_ addConstraint' (typingTypeConstraints tp)
             return $ typingType tp
      else do
        -- Type is polymorphic, so we must instantiate the type and
        -- corresponding type constraints with fresh variables
        let xs = Set.unions $ Set.toList $
                 Set.map (Set.unions . map variables . arguments)
                         (typingTypeConstraints tp)
        let ys :: Set TypeVarId = (variables $ typingType tp)
        -- The set of all type variables occurring in either the type or in the
        -- type constraints:
        let xys = Set.toList $ xs `Set.union` ys
        -- Build a substitution containing fresh variables
        subst <- foldM (\s x -> do y <- getFreshTypeVar
                                   return $ Map.insert x y s) Map.empty xys
        -- Apply the substitution, add constraints, and return type
        mapM_ (addConstraint' . fmap (appSubst subst))
              (typingTypeConstraints tp)
        return $ appSubst subst $ typingType tp
    typeInfAlg ELambda{expLambdaParam = var, expLambdaBody = mtp} = do
      x <- getFreshTypeVar
      -- Add a "constraint" to track potential unifications with x
      let Just (TVar v) = project x
      addTrackingConstraint v
      -- Do type inference of the body, where the variable environment is
      -- extended with a fresh type variable for the parameter
      tp2 <- local (\env -> env{variableEnv = let env' = variableEnv env in
                                              Map.insert var (simpleTyping x) env'}) mtp
      -- Now check if unification has assigned a new type to the newly generated
      -- type variable, by inspecting the associated tracking constraint
      tp1 <- trackTypeVar v
      return $ tp1 ~> tp2
    typeInfAlg EApply{expApplyFn = mtp1, expApplyArg = mtp2} = do
      tp1 <- mtp1
      tp2 <- mtp2
      x <- getFreshTypeVar
      y <- getFreshTypeVar
      -- Check that tp1 is in fact a function type
      subst <- unify [(tp1,x ~> y)]
      let tp11 = appSubst subst x
      let tp12 = appSubst subst y
      let tp2' = appSubst subst tp2
      -- We must check that the type of the argument is a sub type of the
      -- parameter type
      addSubTypeConstraint tp2' tp11
      return tp12
    typeInfAlg EProj{expProjFieldName = f, expProjRecord = mtp} = do
      -- Add a constraint that the inferred type of the record expression has a
      -- field, f, which has the returned type
      tp <- mtp
      x <- getFreshTypeVar
      addFieldConstraint tp f x
      return x
    typeInfAlg EUpdate{expUpdateRecord = mtp1,
                       expUpdateFieldName = f,
                       expUpdateValue = mtp2} = do
      -- Add a constraint that the inferred type of the record expression has a
      -- field, f, which is a super type of the update type
      tp1 <- mtp1
      tp2 <- mtp2
      x <- getFreshTypeVar
      addFieldConstraint tp1 f x
      addSubTypeConstraint tp2 x
      return tp1
    typeInfAlg EBinOp{expBinOp = o,expBinOpArg1 = mtp1,expBinOpArg2 = mtp2} = do
      tp1 <- mtp1
      tp2 <- mtp2
      case o of
        EQ -> processEq tp1 tp2
        LEQ -> processOrder tp1 tp2
        PLUS -> processNum tp1 tp2
        TIMES -> processNum tp1 tp2
        DIV -> processNum tp1 tp2
        AND -> processBool tp1 tp2
        CONS -> do subst <- unify [(tp2, iTList tp1)]
                   return $ appSubst subst tp2
        DPLUS -> do unify [(tp1,iTDuration),(tp2,iTDuration)]
                    return iTDuration
        DTIMES -> do unify [(tp1,iTInt),(tp2,iTDuration)]
                     return iTDuration
        where processEq tp1 tp2 = do
                x <- getFreshTypeVar
                addSubTypeConstraint tp1 x
                addSubTypeConstraint tp2 x
                addEqConstraint x
                return iTBool
              processOrder tp1 tp2 = do
                addOrderConstraint tp1
                unify [(tp2,tp1)]
                return iTBool
              processNum tp1 tp2 = do
                -- tp1 and tp2 must have a common super type
                x <- getFreshTypeVar
                addSubTypeConstraint tp1 x
                addSubTypeConstraint tp2 x
                -- which is a sub type of double
                addSubTypeConstraint x iTReal
                return x
              processBool tp1 tp2 = do
                unify [(tp1,iTBool),(tp2,iTBool)]
                return iTBool
    typeInfAlg EIfThenElse{expCondition = mtp1,
                           expConditionThen = mtp2,
                           expConditionElse = mtp3} = do
      tp1 <- mtp1
      tp2 <- mtp2
      tp3 <- mtp3
      subst <- unify [(tp1,iTBool),(tp3,tp2)]
      return $ appSubst subst tp3
    typeInfAlg ECase{expCase = mtp, expCaseType = mrName, expCases = mtps} = do
      recordEnv <- asks recordTypingEnv
      -- The list of records names which are covered by a case
      let rNames = map caseExpRecordName mtps
      -- Check for exhaustiveness
      rName <- checkExhaustiveness recordEnv rNames mrName
      tp <- mtp
      -- The case expression must have the type which the cases cover
      subst <- unify [(tp,iTRecord rName)]
      tps <- mapM infCase mtps
      subst' <- unify $ genPairs tps
      return $ appSubst (Map.union subst' subst) $ head tps
          where checkExhaustiveness recordEnv rNames mrName = do
                    case mrName of
                      Just rName ->
                          -- Explicit type annotation
                          checkEx recordEnv rName rNames
                      Nothing -> do
                          -- No type annotation, so a super type must be inferred
                          supers <- superTypes recordEnv rNames
                          case supers of
                            [rName] ->
                                 checkEx recordEnv rName rNames
                            _ ->
                                 typeInfError $ CaseCommonSuperTypeError "Unable to infer a (unique) super type"
                checkEx recordEnv rName rNames = do
                    case uncovered recordEnv rName rNames of
                      Left e -> typeInfRunTimeError e 
                      Right [] -> return rName
                      Right u -> typeInfError $ CaseNoneExhaustive u
                superTypes recordEnv rNames =
                  foldM (\r2s r1 -> do
                           supers <- mapM (either (typeInfError . CaseCommonSuperTypeError)
                                                  return .
                                           findCommonSuperTypes recordEnv r1) r2s
                           return $ concat supers)
                        [head rNames]
                        rNames
                infCase CaseExp{caseExpRecordName = rName,
                                caseExpVar = x,
                                caseExpBody = mtp} =
                  local (\env -> env{variableEnv = let env' = variableEnv env in
                                                   Map.insert x (simpleTyping $
                                                                 iTRecord rName) env'})
                        mtp
                genPairs [] = []
                genPairs [_] = []
                genPairs (x:y:xys) = (x,y) : genPairs (y:xys)