{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts.Language.CSL.Interpreter
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module implements the CSL small-step reduction semantics.
--
--------------------------------------------------------------------------------
module Poets.Contracts.Language.CSL.Interpreter
    (
     SubTypeRelation,
     Predefined(..),
     VarEvalEnv,
     emptyPredefined,
     stepC,
     unfold,
     instantiate,
     evalExpr,
     evalExpr',
     evalDeadline,
     normalizeExpr
    ) where

import           Control.Monad                                      hiding
                                                                     (mapM)
import           Control.Monad.Error                                hiding
                                                                     (mapM)
import           Control.Monad.Identity                             hiding
                                                                     (mapM)
import           Control.Monad.Reader                               hiding
                                                                     (mapM)
import           Data.Comp.Ops
import           Data.Comp.Variables
import           Data.List
import           Data.Map                                           (Map)
import qualified Data.Map                                           as Map hiding
                                                                            (Map)
import           Data.Ord                                           (comparing)
import           Data.Traversable                                   (mapM)
import           Data.Traversable
import           Poets.Contracts.Base                               hiding (instantiate)
import           Poets.Contracts.Language.CSL.AST                   as AST hiding
                                                                            (EQ,
                                                                            GT,
                                                                            LT)
import           Poets.Contracts.Language.CSL.Evaluation.Evaluator
import           Poets.Contracts.Language.CSL.Evaluation.Substitute
import           Poets.Contracts.Language.CSL.Exceptions
import           Poets.Contracts.Language.CSL.Render                ()
import           Poets.Data                                         hiding (RunTimeError)
import           Poets.EventLog                                     (Event)
import           Poets.EventLog.Names                               (contractEventFieldContractId)
import           Prelude                                            hiding
                                                                     (mapM)

-- |The interpreter monad. Clause and function definitions are stored in the
-- environment, as well as the current time (that is, the time with respect to
-- which deadlines are calculated).
type IM c = ReaderT (Environment c) (ErrorT CSLError Identity)

-- |Run a computation in the interpreter monad.
evalIM :: Environment c -> IM c a -> Either CSLError a
evalIM env m = runIdentity (runErrorT $ runReaderT m env)

-- |The environment used in CSL run-time monitoring.
data Environment c = Environment{
      intTime       :: DateTime,
      intClauseDefs :: ClauseDefs c,
      intPredefined :: Predefined,
      intIsSubType  :: SubTypeRelation,
      intEventLog   :: [Event]
    }

-- |The variable evaluation environment used in CSL expression evaluation.
type VarEvalEnv = EvalEnv ValueSig

-- |Predefined values and functions. Since predefined values include reports,
-- they are parametrised over the event log.
data Predefined = Predefined{
      preVals :: [Event] -> VarEvalEnv,
      preExps :: FunctionDefCores
    }

emptyPredefined :: Predefined
emptyPredefined = Predefined{preVals = const Map.empty,
                             preExps = Map.empty}

-- |The substitution type used in CSL expressions. (Source code information is
-- not included, since substitution values are not read from a file.)
type Substitution f = Subst f Var

-- |Lookup a clause definition in the environment.
lookupClauseDef :: TemplateName -> IM c (ClauseDef c)
lookupClauseDef tName = do
  cDefs <- asks intClauseDefs
  maybe (templateUndefError tName) return (Map.lookup tName cDefs)

-- |Get the current time.
getTime :: IM c DateTime
getTime = asks intTime

-- |Set the time to be used in an interpretation computation.
withTime :: DateTime -> IM c a -> IM c a
withTime dt = local (\env -> env{intTime = dt})


--------------------------------------------------------------------------------
-- CSL small-step reduction semantics
--------------------------------------------------------------------------------

-- |Run-time monitoring via residuation. Register the given transaction against
-- the given clause. The boolean return type indicates whether the transaction
-- was expected or not.
stepRes :: (Step c c, SubstC c c)
              => DateTime -> Transaction -> Residue c -> IM c (Residue c, Bool)
stepRes trTimeStamp tr r =
    case r of
      Left Breach{} ->
          -- Clause is breached, so the transaction is not expected
          return (r, False)
      Right c ->
          step trTimeStamp tr (unTerm c)

-- |Small-step relation.
class (SubstC f g, SubstP f g) => Step f g where
    step :: DateTime -> Transaction -> f (Term g) -> IM g (Residue g, Bool)

instance (Step f h, Step g h) => Step (f :+: g) h where
    step dt tr (Inl x) = step dt tr x
    step dt tr (Inr x) = step dt tr x

-- Dummy instance
instance HasVars CoreClause Var where

-- |Small-step instance for (core) clauses.
instance (CoreClause :<: c, Step c c, VUnit :<: c, Val :<: c, CoreExp :<: c,
          HasVars c Var, Traversable c) => Step CoreClause c where
    step trTimeStamp tr c = do
      trType <- either runTimeError return $ extractTransactionType tr
      case c of
        Fulfilment{} ->
          -- Clause is fulfilled, so the transaction is not expected
          return (Right $ inject c, False)
        Obligation{clauseResponsible = responsible,
                   clauseTransactionType = expectedTrType,
                   clauseBinders = b,
                   clauseRemainderVar = rVar,
                   clausePredicate = pred,
                   clauseDeadline = deadline,
                   clauseContinuation = c'} -> do
          isSubType <- asks intIsSubType
          party <- evalExprIM responsible
          (dt1,dt2) <- evalDeadlineIM deadline
          if trTimeStamp > dt2 then do
              curTime <- getTime
              -- The clause has been breached due to a deadline expiration
              let descr = "Deadline expired; expected a '" ++ expectedTrType ++
                          "' transaction on or before " ++ show dt2
              return (Left Breach{breachTime = max curTime dt2,
                                  breachParties = [(party, descr)]}, False)
          else
              -- The deadline has not passed, so first check if the transaction
              -- types match, or if the transaction is too early
              if not (isSubType trType expectedTrType) || trTimeStamp < dt1 then do
                  -- Update the relative deadline
                  d' <- updateDeadline trTimeStamp deadline
                  return (Right $ inject c{clauseDeadline = d'}, False)
              else do
                  -- Build substitution for predicate expression
                  substp <- mapM (\f -> either runTimeError
                                               (return . deepInject)
                                               (extractField f tr))
                                 (Map.fromList b)
                  v <- evalExprIM $ appSubst substp pred
                  -- Now check if the transaction fulfils the constraints
                  case project v of
                    Just (VBool False) -> do
                      -- Update the relative deadline
                      d' <- updateDeadline trTimeStamp deadline
                      return (Right $ inject c{clauseDeadline = d'}, False)
                    _ -> do
                      -- Transaction fulfils the conditions of the obligation
                      -- so the residual clause is the continuation
                      let subst = addRemainderSubst dt2 trTimeStamp rVar substp
                      return (Right $ substClause subst c', True)
        ExternalChoice{clauseTransactionType = expectedTrType,
                       clauseBinders = b,
                       clauseRemainderVar = rVar,
                       clausePredicate = pred,
                       clauseDeadline = deadline,
                       clauseContinuation = c1,
                       clauseElse = c2} -> do
          isSubType <- asks intIsSubType
          (dt1,dt2) <- evalDeadlineIM deadline
          if trTimeStamp > dt2 then do
              curTime <- getTime
              -- The deadline has passed, so the transaction must be matched
              -- against the else clause
              withTime (max curTime dt2) $ stepRes trTimeStamp tr (Right c2)
          else
              -- The deadline has not passed, so first check if the transaction
              -- types match, or if the transaction is too early
              if not (isSubType trType expectedTrType) || trTimeStamp < dt1 then do
                  -- Update the relative deadline
                  d' <- updateDeadline trTimeStamp deadline
                  return (Right $ inject c{clauseDeadline = d'}, False)
              else do
                  -- Build substitution for predicate expression
                  substp <- mapM (\f -> either runTimeError
                                               (return . deepInject)
                                               (extractField f tr))
                                 (Map.fromList b)
                  v <- evalExprIM $ appSubst substp pred
                  -- Now check if the transaction fulfils the constraints
                  case project v of
                    Just (VBool False) -> do
                      -- Update the relative deadline
                      d' <- updateDeadline trTimeStamp deadline
                      return (Right $ inject c{clauseDeadline = d'}, False)
                    _ -> do
                      -- Transaction fulfils the conditions of the obligation
                      -- so the residual clause is the continuation
                      let subst = addRemainderSubst dt2 trTimeStamp rVar substp
                      return (Right $ substClause subst c1, True)
        InternalChoice{clauseCondition = e,
                       clauseThen = c1,
                       clauseElse = c2} -> do
          b <- evalExprIM e
          case project b of
            Just (VBool True) -> do
                (r1, b1) <- stepRes trTimeStamp tr (Right c1)
                return (if b1 then r1 else Right $ inject c, b1)
            Just (VBool False) -> do
                (r2, b2) <- stepRes trTimeStamp tr (Right c2)
                return (if b2 then r2 else Right $ inject c, b2)
            _ ->
                unexpectedValueError "boolean"
        And{clauseLeft = c1, clauseRight = c2} -> do
          (r1, b1) <- stepRes trTimeStamp tr (Right c1)
          (r2, b2) <- stepRes trTimeStamp tr (Right c2)
          -- Combine the two residues
          case (r1, r2) of
            (Right c1', Right  c2') ->
                -- Neither of the two clauses are breached
                return (Right $ inject (And{clauseLeft = c1',
                                            clauseRight = c2'}
                                        :: CoreClause (Term c)), b1 || b2)
            (Left breach, Right _) ->
                -- Only the left conjunct is breached
                return (Left breach, b1 || b2)
            (Right _, Left breach) ->
                -- Only the right conjunct is breached
                return (Left breach, b1 || b2)
            (Left breach1, Left breach2) ->
                -- Both conjuncts are breached, so take the earliest breach
                case comparing breachTime breach1 breach2 of
                  LT -> return (Left breach1, b1 || b2)
                  GT -> return (Left breach2, b1 || b2)
                  EQ -> return (Left breach1{breachParties =
                                                 nub $ breachParties breach1 ++
                                                       breachParties breach2},
                                b1 || b2)
        Or{clauseLeft = c1, clauseRight = c2} -> do
          (r1, b1) <- stepRes trTimeStamp tr (Right c1)
          (r2, b2) <- stepRes trTimeStamp tr (Right c2)
          -- Combine the two residues
          case (r1, r2) of
            (Right c1', Right c2') ->
                -- Neither of the two clauses are breached
                return (Right $ inject (Or{clauseLeft = c1',
                                           clauseRight = c2'}
                                        :: CoreClause (Term c)), b1 || b2)
            (Left _, Right c2') ->
                -- Only the left disjunct is breached
                return (Right c2', b1 || b2)
            (Right c1', Left _) ->
                -- Only the right disjunct is breached
                return (Right c1', b1 || b2)
            (Left breach1, Left breach2) ->
              -- Both disjuncts are breached, so take the latest breach
              case comparing breachTime breach1 breach2 of
                LT -> return (Left breach2, b1 || b2)
                GT -> return (Left breach1, b1 || b2)
                EQ -> do
                  let parties = nub $ breachParties breach1 ++
                                      breachParties breach2
                  when (length parties > 1)
                       (runTimeError $ "The breaching parties differ in a " ++
                                       "disjunction")
                  return (Left breach1{breachParties = parties}, b1 || b2)
        Case{clauseCaseExpr = e, clauseCases = cs} -> do
          isSubType <- asks intIsSubType
          v <- evalExprIM e
          case extractRecord v of
            Right r ->
                -- Go through each case to find the first match
                case find (isSubType (vrecordName r) .
                           clauseCaseRecordName) cs of
                  Just c -> do
                    -- Bind the case value and continue reduction
                    let c' = substClause (Map.singleton (clauseCaseVar c)
                                                        (deepInject v))
                                         (clauseCaseBody c)
                    stepRes trTimeStamp tr (Right c')
                  Nothing ->
                      matchError $ vrecordName r
            Left _ ->
                unexpectedValueError "record"
        Instantiate{clauseClauseTemplate = tName,
                    clauseArgs = args,
                    clausePartyArgs = partyArgs} -> do
          -- First unfold template definition
          c' <- unfoldInstantiation tName args partyArgs
          -- Then step w.r.t. the template body
          stepRes trTimeStamp tr (Right c')
      where addRemainderSubst deadline curTime x =
                let diff = deadline `dateTimeDiff` curTime in
                Map.insert x (iVDuration diff)

-- Dummy instance
instance Step VUnit f where
    step _ _ _ = runTimeError "Unexpected Unit in step"

-- Dummy instance
instance Step Val f where
    step _ _ _ = runTimeError "Unexpected Val in step"

-- Dummy instance
instance Step CoreExp f where
    step _ _ _ = runTimeError "Unexpected CoreExp in step"

-- |Given a new time, update a deadline.
updateDeadline :: (VUnit :<: f, Val :<: f, CoreExp :<: f)
                  => DateTime -> Deadline (Term f) -> IM f (Deadline (Term f))
updateDeadline newTime d = do
  oldTime <- getTime
  Just (VDuration vd2) <- liftM project $ evalExprIM $ deadlineAfter d
  let vd2' = addDurationToDateTime vd2 oldTime `dateTimeDiff` newTime
  return Deadline{deadlineWithin = deadlineWithin d,
                  deadlineAfter = iVDuration vd2'}

-- |Unfold the definition of a template by substituting (party) values into the
-- body of the template.
unfoldInstantiation :: (SubstC f f, SubstP f f, Val :<: f,
                        VUnit :<: f, CoreExp :<: f)
                       => TemplateName -> [Term f] -> [Term f] -> IM f (Term f)
unfoldInstantiation tName args partyArgs = do
  cDef <- lookupClauseDef tName
  let vars = fst $ unzip $ clauseDefParams cDef
  let partyVars = fst $ unzip $ clauseDefPartyParams cDef
  when (length args /= length vars) (templateArgsError tName)
  when (length partyArgs /= length partyVars) (templatePartyArgsError tName)
  vArgs <- mapM evalExprIM args
  vPartyArgs <- mapM evalExprIM partyArgs
  let subst = Map.fromList $ zip vars (map deepInject vArgs)
  let partySubst = Map.fromList $ zip partyVars (map deepInject vPartyArgs)
  -- Substitute arguments into template body. NB: party parameters are
  -- also substituted
  let c' = substClause (subst `Map.union` partySubst) (clauseDefBody cDef)
  -- Substitute party arguments
  return $ substClauseP partySubst c'

-- |Clause unfolding.
class (SubstC f g, SubstP f g) => Unfold f g where
    unf :: f (Term g) -> IM g (Term g)

instance (Unfold f h, Unfold g h) => Unfold (f :+: g) h where
    unf (Inl x) = unf x
    unf (Inr x) = unf x

-- |Clause unfolding for (core) clauses.
instance (CoreClause :<: c, Unfold c c, VUnit :<: c, Val :<: c, CoreExp :<: c,
          HasVars c Var, Traversable c) => Unfold CoreClause c where
    unf c@Fulfilment{} = return (inject c)
    unf c@Obligation{} = return (inject c)
    unf c@ExternalChoice{clauseElse = c2} = do
      c2' <- unf (unTerm c2)
      return (inject c{clauseElse = c2'})
    unf InternalChoice{clauseCondition = e,
                       clauseThen = c1,
                       clauseElse = c2} = do
      b <- evalExprIM e
      case project b of
        Just (VBool True)  -> unf (unTerm c1)
        Just (VBool False) -> unf (unTerm c2)
        _                  -> unexpectedValueError "boolean"
    unf And{clauseLeft = c1, clauseRight = c2} = do
      c1' <- unf (unTerm c1)
      c2' <- unf (unTerm c2)
      return $ inject (And{clauseLeft = c1', clauseRight = c2'}
                           :: CoreClause (Term c))
    unf Or{clauseLeft = c1, clauseRight = c2} = do
      c1' <- unf (unTerm c1)
      c2' <- unf (unTerm c2)
      return $ inject (Or{clauseLeft = c1', clauseRight = c2'}
                           :: CoreClause (Term c))
    unf Case{clauseCaseExpr = e, clauseCases = cs} = do
      isSubType <- asks intIsSubType
      v <- evalExprIM e
      case extractRecord v of
        Right r ->
            -- Go through each case to find the first match
            case find (isSubType (vrecordName r) . clauseCaseRecordName) cs of
              Just c -> do
                -- Bind the case value and continue reduction
                let c' = substClause (Map.singleton (clauseCaseVar c)
                                                    (deepInject v))
                                     (clauseCaseBody c)
                unf (unTerm c')
              Nothing ->
                  matchError $ vrecordName r
        Left _ ->
            unexpectedValueError "record"
    unf Instantiate{clauseClauseTemplate = tName,
                    clauseArgs = args,
                    clausePartyArgs = partyArgs} = do
      -- First unfold template definition
      c' <- unfoldInstantiation tName args partyArgs
      -- Then unfold recursively
      unf (unTerm c')

-- Dummy instance
instance Unfold VUnit f where
    unf _ = runTimeError "Unexpected Unit in unf"

-- Dummy instance
instance Unfold Val f where
    unf _ = runTimeError "Unexpected Val in unf"

-- Dummy instance
instance Unfold CoreExp f where
    unf _ = runTimeError "Unexpected CoreExp in unf"

-- |Unfold top-level instantiations, and check for breaches of contract.
unfoldClause :: (Step c c, Unfold c c) => Residue c -> IM c (Residue c)
unfoldClause r = do
  curTime <- getTime
  -- First check for breach
  (r', b) <- stepRes curTime silentTr r
  when b updateUnexpectedMatch
  case r' of
    Left Breach{} -> return r
    Right c       -> liftM Right $ unf (unTerm c)
    where silentTr = iVRecord VR{vrecordName = "", vrecordFields = newFields []}


--------------------------------------------------------------------------------
-- Expression evaluation
--------------------------------------------------------------------------------

-- |Evaluate a CSL expression inside the IM monad.
evalExprIM :: (Val :<: f, VUnit :<: f, CoreExp :<: f) => Term f -> IM f Poets.Data.Value
evalExprIM e = do
  isSubType <- asks intIsSubType
  pDef <- asks intPredefined
  events <- asks intEventLog
  either throwError return (evalExpr' isSubType events pDef e)

-- |Evaluate a deadline expression inside the IM monad.
evalDeadlineIM :: (Val :<: f, VUnit :<: f, CoreExp :<: f)
                  => Deadline (Term f) -> IM f (DateTime, DateTime)
evalDeadlineIM d = do
  isSubType <- asks intIsSubType
  pDef <- asks intPredefined
  events <- asks intEventLog
  dt <- getTime
  either throwError return (evalDeadline isSubType events pDef dt d)


--------------------------------------------------------------------------------
-- Substitutions
--------------------------------------------------------------------------------

-- |Substitution of values into clauses.
class SubstC f g where
    substC :: SubstC g g => Substitution g -> f (Term g) -> f (Term g)

instance (SubstC f h, SubstC g h) => SubstC (f :+: g) h where
    substC s (Inl x) = Inl $ substC s x
    substC s (Inr x) = Inr $ substC s x

substClause :: SubstC f f => Substitution f -> Term f -> Term f
substClause s c = Term $ substC s (unTerm c)

instance (HasVars f Var, Traversable f) => SubstC CoreClause f where
    substC subst c =
        case c of
          Fulfilment{} ->
              c
          Obligation{clauseBinders = b,
                     clauseRemainderVar = x,
                     clauseDeadline = d,
                     clausePredicate = p,
                     clauseContinuation = c'} ->
              -- List of variables bound in clause
              let vars = map fst b in
              -- Restricted substitution for predicate
              let substp = res subst vars in
              -- Restricted substitution for continuation
              let substc = res subst (x:vars) in
              c{clauseDeadline = fmap (appSubst subst) d,
                clausePredicate = appSubst substp p,
                clauseContinuation = substClause substc c'}
          ExternalChoice{clauseBinders = b,
                         clauseRemainderVar = x,
                         clauseDeadline = d,
                         clausePredicate = p,
                         clauseContinuation = c1,
                         clauseElse = c2} ->
              -- List of variables bound in clause
              let vars = map fst b in
              -- Restricted substitution for predicate
              let substp = res subst vars in
              -- Restricted substitution for continuation
              let substc = res subst (x:vars) in
              c{clauseDeadline = fmap (appSubst subst) d,
                clausePredicate = appSubst substp p,
                clauseContinuation = substClause substc c1,
                clauseElse = substClause subst c2}
          InternalChoice{clauseCondition = e,
                         clauseThen = c1,
                         clauseElse = c2} ->
              c{clauseCondition = appSubst subst e,
                clauseThen = substClause subst c1,
                clauseElse = substClause subst c2}
          And{clauseLeft = c1, clauseRight = c2} ->
              c{clauseLeft = substClause subst c1,
                clauseRight = substClause subst c2}
          Or{clauseLeft = c1, clauseRight = c2} ->
              c{clauseLeft = substClause subst c1,
                clauseRight = substClause subst c2}
          Case{clauseCaseExpr = e, clauseCases = cs} ->
              c{clauseCaseExpr = appSubst subst e,
                clauseCases = map (\c -> c{clauseCaseBody = substClause (res subst [clauseCaseVar c]) $ clauseCaseBody c}) cs}
          Instantiate{clauseArgs = args} ->
              c{clauseArgs = map (appSubst subst) args}

-- Dummy instance
instance SubstC VUnit f where
    substC _ = id

-- Dummy instance
instance SubstC Val f where
    substC _ = id

-- Dummy instance
instance SubstC CoreExp f where
    substC _ = id


-- |Substitution of parties into clauses.
class SubstP f g where
    substP :: SubstP g g => Substitution g -> f (Term g) -> f (Term g)

instance (SubstP f h, SubstP g h) => SubstP (f :+: g) h where
    substP s (Inl x) = Inl $ substP s x
    substP s (Inr x) = Inr $ substP s x

substClauseP :: SubstP f f => Substitution f -> Term f -> Term f
substClauseP s c = Term $ substP s (unTerm c)

instance (HasVars f Var, Traversable f) => SubstP CoreClause f where
    substP subst c =
        case c of
          Fulfilment{} ->
              c
          Obligation{clauseResponsible = r, clauseContinuation = c'} ->
              c{clauseResponsible = appSubst subst r,
                clauseContinuation = substClauseP subst c'}
          ExternalChoice{clauseContinuation = c1, clauseElse = c2} ->
              c{clauseContinuation = substClauseP subst c1,
                clauseElse = substClauseP subst c2}
          InternalChoice{clauseThen = c1, clauseElse = c2} ->
              c{clauseThen = substClauseP subst c1,
                clauseElse = substClauseP subst c2}
          And{clauseLeft = c1, clauseRight = c2} ->
              c{clauseLeft = substClauseP subst c1,
                clauseRight = substClauseP subst c2}
          Or{clauseLeft = c1, clauseRight = c2} ->
              c{clauseLeft = substClauseP subst c1,
                clauseRight = substClauseP subst c2}
          Case{clauseCases = cs} ->
              c{clauseCases = map (\c -> c{clauseCaseBody = substClauseP subst $ clauseCaseBody c}) cs}
          Instantiate{clausePartyArgs = partyArgs} ->
              c{clausePartyArgs = map (appSubst subst) partyArgs}

-- Dummy instance
instance SubstP VUnit f where
    substP _ = id

-- Dummy instance
instance SubstP Val f where
    substP _ = id

-- Dummy instance
instance SubstP CoreExp f where
    substP _ = id

-- Restrict a substitution, subst, by removing bindings to variables in vars
res :: Substitution f -> [Var] -> Substitution f
res subst vars = Map.filterWithKey (\x _ -> notElem x vars) subst


--------------------------------------------------------------------------------
-- Error messages
--------------------------------------------------------------------------------

-- Throw a runtime error
runTimeError :: String -> IM c a
runTimeError = throwError . RunTimeError

updateUnexpectedMatch :: IM c a
updateUnexpectedMatch =
    runTimeError "Unexpected match of silent action"

templateUndefError :: TemplateName -> IM c a
templateUndefError tName =
    runTimeError $ "Template '" ++ tName ++ "' is undefined"

unexpectedValueError :: String -> IM c a
unexpectedValueError tp =
    runTimeError $ "Unexpected value: expected a " ++ tp ++ " value"

matchError rName =
    runTimeError $ "Pattern matching not exhaustive, no pattern for '" ++
                   rName ++ "'"
templateArgsError :: TemplateName -> IM c a
templateArgsError tName =
    runTimeError $ "Argument mismatch in instantiation of template '" ++
                   tName ++ "'"

templatePartyArgsError :: TemplateName -> IM c a
templatePartyArgsError tName =
    runTimeError $ "Party argument mismatch in instantiation of template '" ++
                   tName ++ "'"

contractDefUndefinedError :: TemplateName -> Either CSLError a
contractDefUndefinedError = throwError . ContractTemplateNotFound

contractDefTypeError :: TemplateName
                     -> ContractType
                     -> ContractType
                     -> Either CSLError a
contractDefTypeError tName cType1 cType2 =
    throwError $ ContractTemplateTypeMismatch tName cType1 cType2


--------------------------------------------------------------------------------
-- Exported functions
--------------------------------------------------------------------------------

-- |Match a transaction against a clause, and return the updated clause/breach.
-- The returned boolean indicates whether there was a \"match\" or not.
stepC :: SubTypeRelation -- ^Sub type relation.
      -> [Event] -- ^The event log.
      -> Predefined -- ^Predefined values/expressions.
      -> DateTime -- ^The time stamp of the transaction.
      -> Transaction -- ^The transaction to match.
      -> Contract Closure -- ^The contract to match against.
      -> Either CSLError (Bool, Residue ClauseCoreSig)
stepC isSubType events pDef dt tr c = do
  (env, r) <- buildEnvRes isSubType events pDef c
  evalIM env $ do (r',b) <- stepRes dt tr r; return (b,r')
--                   r'' <- unfoldClause r';
--                   return (b, r'')}

-- |Unfold a clause by unfolding all top-level instantiations and top-level
-- conditionals.
unfold :: SubTypeRelation -- ^Sub type relation.
       -> [Event] -- ^The event log.
       -> Predefined -- ^Predefined values/expressions.
       -> Contract Closure -- ^The contract to unfold.
       -> Either CSLError (Residue ClauseCoreSig)
unfold isSubType events pDef c = do
  (env, r) <- buildEnvRes isSubType events pDef c
  evalIM env (unfoldClause r)

-- |Instantiate a new CSL contract, by supplying the contract definitions, the
-- library functions, and the meta data of the contract to instantiate (the meta
-- data includes all parameters, and the start date and the name of the contract
-- template to instantiate).
instantiate :: SubTypeRelation -- ^Sub type relation.
            -> ContractDefCores -- ^The contract definitions.
            -> ContractMetaData -- ^The contract meta data.
            -> ContractId -- ^The ID of the contract.
            -> Either CSLError (Either Breach (Contract Closure))
instantiate isSubType cDefs metaData cId = do
  tName <- either (Left . RunTimeError) return $ extractTemplateName metaData
  cType <- either (Left . RunTimeError) return $ extractContractType metaData
  startDate <- either (Left . RunTimeError) return $ extractStartDate metaData
  case Map.lookup tName cDefs of
    Nothing ->
        contractDefUndefinedError tName
    Just cDef -> do
      -- We now need to check that the contract type specified in the contract
      -- meta data, cType, matches that of the contract template.
      unless (isSubType cType (contractDefType cDef))
             (contractDefTypeError tName (contractDefType cDef) cType)
      let fDefs = foldl (\fDefs fDef ->
                             Map.insert (functionName fDef) fDef fDefs)
                        Map.empty
                        (contractDefFunDefs cDef)
      subst <- liftM (Map.insert contractEventFieldContractId (iVInt cId))
                     (buildSubst metaData)
      let cl = Closure{closureClause = substClauseP subst $
                                       substClause subst $ contractDefBody cDef,
                       closureClauseDefs = contractDefClauseDefs cDef,
                       closureFunctionDefs = fDefs}
      let c = Contract{contractLastUpdate = startDate,
                       contractContent = cl,
                       contractMetaData = metaData}
      -- Update the newly created contract to check for immediate breaches
--      res <- unfold isSubType events pDef c
      return $ Right c
    where buildSubst :: ContractMetaData -> Either CSLError (Substitution ClauseCoreSig)
          buildSubst metaData = do
            fields <- either (Left . RunTimeError) (return . fieldsMap' . vrecordFields) (extractRecord metaData)
            return $ Map.map deepInject fields

buildEnvRes :: SubTypeRelation
            -> [Event]
            -> Predefined
            -> Contract Closure
            -> Either CSLError (Environment ClauseCoreSig, Residue ClauseCoreSig)
buildEnvRes isSubType events pDef c = do
  let cl = contractContent c
  let env = Environment{intClauseDefs = closureClauseDefs cl,
                        intPredefined =
                            pDef{preExps = Map.union (closureFunctionDefs cl)
                                                     (preExps pDef)},
                        intIsSubType = isSubType,
                        intEventLog = events,
                        intTime = contractLastUpdate c}
  return (env, Right $ closureClause cl)

-- |Evaluate a CSL expression to a CSL value.
evalExpr :: (VUnit :<: f, Val :<: f, CoreExp :<: f) =>
            SubTypeRelation -- ^Sub type relation.
         -> [Event] -- ^The event log.
         -> Predefined -- ^Predefined values/expressions.
         -> Term f -- ^Expression to evaluate.
         -> Either CSLError AST.Value
evalExpr isSubType events pDef e' =
  undefined
    -- case deepProject3 e' of
    --   Just (e :: ExprCore) ->
    --       either (throwError . RunTimeError)
    --              return
    --              (evalTerm (preVals pDef events) isSubType (inline (preExps pDef) e))
    --   Nothing ->
    --       Left $ RunTimeError "Expected core expression in evalExpr"

-- |Evaluate a CSL expression to a POETS (i.e., non-functional) value.
evalExpr' :: (VUnit :<: f, Val :<: f, CoreExp :<: f) =>
             SubTypeRelation -- ^Sub type relation.
          -> [Event] -- ^The event log.
          -> Predefined -- ^Predefined values/expressions.
          -> Term f -- ^Expression to evaluate.
          -> Either CSLError Poets.Data.Value
evalExpr' isSubType events pDef e = do
  v <- evalExpr isSubType events pDef e
  maybe (throwError $ RunTimeError "Failed to extract POETS value")
        return
        (deepProject v)

-- |Evaluate a CSL deadline expression. The returned pair is the lower and upper
-- bound of the deadline, calculated w.r.t. the supplied base time.
evalDeadline :: (VUnit :<: f, Val :<: f, CoreExp :<: f) =>
                SubTypeRelation -- ^Sub type relation.
             -> [Event] -- ^The event log.
             -> Predefined -- ^Predefined values/expressions.
             -> DateTime -- ^Base time.
             -> Deadline (Term f) -- ^Deadline to evaluate.
             -> Either CSLError (DateTime, DateTime)
evalDeadline isSubType events pDef dt Deadline{deadlineWithin = e1,
                                               deadlineAfter = e2} = do
  Just (VDuration vd1) <- liftM project $ evalExpr' isSubType events pDef e1
  Just (VDuration vd2) <- liftM project $ evalExpr' isSubType events pDef e2
  let lowerDelta = addDurationToDateTime vd2 dt
  let upperDelta = addDurationToDateTime vd1 (addDurationToDateTime vd2 dt)
  return (lowerDelta, upperDelta)

-- |Normalization by evaluation + beta reduction. All function definitions are
-- inlined, so the resulting expression is a closed term. For now this function
-- is very ad hoc.
normalizeExpr :: (Evaluator f ValueSig,
                  Val :<: f, VUnit :<: f, CoreExp :<: f, EqF f,
                  Traversable f, HasVars f Var) =>
                 SubTypeRelation -- ^Sub type relation.
              -> VarEvalEnv -- ^Predefined values.
              -> FunctionDefCores -- ^Function definitions to inline.
              -> Term f -- ^The term to normalize.
              -> Term f
normalizeExpr isSubType env fDefs e = fix (transform norm) $ inline fDefs e
    where fix f x = if f x == x then x else fix f (f x)
          norm e = case evalTerm env isSubType e of
                     Left _ ->
                         norm' e
                     Right (val :: AST.Value) ->
                       undefined
                         -- case deepProject2 val of
                         --   Just (v :: Term (VUnit :+: Val)) -> deepInject2 v
                         --   Nothing                          -> norm' e
          -- Ad hoc normalization
          norm' e = normApply e
          normApply e = case project e of
                          Just (EApply x y) ->
                              case project x of
                                Just (ELambda v b) ->
                                    -- beta reduction
                                    appSubst (Map.singleton v y) b
                                _ -> e
                          _ -> normProj e
          normProj e = case project e of
                         Just (EProj f x) ->
                             case project x of
                               Just (VRecord vr) ->
                                   either (const e) id $ lookupField f vr
                               _ -> e
                         _ -> normUpdate e
          normUpdate e = case project e of
                           Just (EUpdate x f y) ->
                               case project x of
                                 Just (VRecord vr) -> iVRecord $ setField f y vr
                                 _ -> e
                           _ -> normIfThenElse e
          normIfThenElse e = case project e of
                               Just (EIfThenElse x y z) ->
                                   case project x of
                                     Just (VBool True)  -> y
                                     Just (VBool False) -> z
                                     _                  -> e
                               _ -> normCase e
          normCase e = case project e of
                         Just (ECase c _ cs) ->
                             case project c of
                               Just (VRecord vr) ->
                                   case find (isSubType (vrecordName vr) . caseExpRecordName) cs of
                                     Just (CaseExp _ x b) ->
                                         appSubst (Map.singleton x c) b
                                     _ -> e
                               _ -> e
                         _ -> e

-- |Inline a set of function definitions in an expression.
inline :: forall f. (Val :<: f, VUnit :<: f, CoreExp :<: f, EqF f,
                     Traversable f, HasVars f Var) => FunctionDefCores
       -> Term f -> Term f
inline fDefs e = repeatSubst e
    where subst :: Map Var (Term f)
          subst = undefined -- Map.map (deepInject3 . functionExp) fDefs
          repeatSubst e =
              let e' = appSubst subst e in
              if e /= e' then repeatSubst e' else e'
