{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, TypeOperators,
  FlexibleInstances, TypeSynonymInstances, ScopedTypeVariables,
  FlexibleContexts, TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts.Language.CSL.Evaluation.Evaluator
-- Copyright   :  3gERP, 2012
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines (big-step) evaluation of CSL expressions.
--
--------------------------------------------------------------------------------
module Poets.Contracts.Language.CSL.Evaluation.Evaluator
    (
     EvalEnv,
     SubTypeRelation,
     Evaluator,
     evalTerm
    ) where

import Prelude hiding (EQ, LT, GT)
import Control.Monad.Error
import Control.Monad.Reader
import Data.List hiding (map, foldr, foldl)
import qualified Data.Map as Map
import Data.Comp
import Data.Comp.Derive
import Data.Traversable as T
import Poets.Data.Value
import Poets.Data.Value.Utils
import Poets.Contracts.Language.CSL.AST.Exp
import Poets.Contracts.Language.CSL.AST.RecordFieldName

-- |Binding of variables/identifiers to their value.
type EvalEnv v = Map.Map Var (Term v)

-- |Sub type relation.
type SubTypeRelation = RecordName -> RecordName -> Bool

-- |The environment used for evaluation.
data Env v = Env{envEvalEnv :: EvalEnv v, envSubTypeRel :: SubTypeRelation}

-- |Evaluation Monad.
type EI v = ReaderT (Env v) (Either String)

-- |Execute evaluation monad and obtain the result.
runEI :: EI v a -> Env v -> Either String a
runEI = runReaderT

-- |Built-in fields for time values.
data TimeField = Hour | Minute | Second | Microsecond

instance Show TimeField where
    show Hour        = "hour"
    show Minute      = "minute"
    show Second      = "second"
    show Microsecond = "microsecond"

parseTimeField :: String -> Maybe TimeField
parseTimeField f
    | f == show Hour = return Hour
    | f == show Minute = return Minute
    | f == show Second = return Second
    | f == show Microsecond = return Microsecond
    | otherwise = Nothing

-- |Built-in fields for date values.
data DateField = Year | Month | Day

instance Show DateField where
    show Year  = "year"
    show Month = "month"
    show Day   = "day"

parseDateField :: String -> Maybe DateField
parseDateField f
    | f == show Year = return Year
    | f == show Month = return Month
    | f == show Day = return Day
    | otherwise = Nothing

-- |The evaluation algebra. An instance, 'Evaluator' @f@ @v@, means that terms
-- over the signature @f@ can be evaluated to terms over the signature @v@.
class Evaluator f v where
    evalAlg :: Alg f (EI v (Term v))

$(derive [liftSum] [''Evaluator])

-- |Evaluate a term.
evalTerm :: (Functor f, Evaluator f v) => EvalEnv v
         -> SubTypeRelation
         -> Term f
         -> Either String (Term v)
evalTerm env rel = (`runEI` Env env rel) . cata evalAlg

instance (Val :<: v) => Evaluator Val v where
    evalAlg v = liftM inject (T.sequence v)

instance (VUnit :<: v) => Evaluator VUnit v where
    evalAlg VUnit = return iVUnit

instance Evaluator RecordFieldName v where
    evalAlg (RecordFieldName _) =
        throwError "Cannot evaluate a record field name"

instance (Val :<: v, VFun :<: v, EqF v) => Evaluator CoreExp v where
    evalAlg EVar{expVar = x} = do
      varEnv <- asks envEvalEnv
      maybe (evalFreeVarError x) return (Map.lookup x varEnv)
    evalAlg ELambda{expLambdaParam = var, expLambdaBody = e} = do
      env <- ask
      return $ iVFun $ \v ->
          let evalEnv' = Map.insert var v (envEvalEnv env) in
          runEI e env{envEvalEnv = evalEnv'}
    evalAlg EApply{expApplyFn = mf, expApplyArg = ma} = do
      f <- mf
      a <- ma
      case project f of
        Just (VFun f') -> either throwError return (f' a)
        _ -> evalFunctionError
    evalAlg EProj{expProjFieldName = f, expProjRecord = mv} = do
      v <- mv
      case project v of
        Just (VRecord vr) ->
            either throwError return (lookupField f vr)
        Just (VDate d) -> do
            let (y, m, da) = dateToYearMonthDay d
            field <- maybe (evalDateError f) return (parseDateField f)
            case field of
              Year  -> return (iVInt $ fromInteger y)
              Month -> return (iVInt m)
              Day   -> return (iVInt da)
        Just (VTime t) -> do
            let (h, m, s, ms) = timeToHourMinSecMicroSec t
            field <- maybe (evalTimeError f) return (parseTimeField f)
            case field of
              Hour        -> return (iVInt h)
              Minute      -> return (iVInt m)
              Second      -> return (iVInt s)
              Microsecond -> return (iVInt ms)
        Just (VDateTime dt) ->
            let (d, t) = dateTimeToDateTime dt in
            let (y, m, da) = dateToYearMonthDay d in
            let (h, mi, s, ms) = timeToHourMinSecMicroSec t in
            case parseDateField f of
              Just Year  -> return (iVInt $ fromInteger y)
              Just Month -> return (iVInt m)
              Just Day   -> return (iVInt da)
              _ -> case parseTimeField f of
                     Just Hour        -> return (iVInt h)
                     Just Minute      -> return (iVInt mi)
                     Just Second      -> return (iVInt s)
                     Just Microsecond -> return (iVInt ms)
                     _                -> evalDateTimeError f
        _ -> evalRecordError
    evalAlg EUpdate{expUpdateRecord = mr,
                    expUpdateFieldName = f,
                    expUpdateValue = mv} = do
      r <- mr
      v <- mv
      let extractInt x = case project x of
                           Just (VInt n) -> return n
                           _ -> throwError $ evalExpectedError "integer"
      let date y m d = maybe (throwError "Failed to create date")
                             return
                             (createDate y m d)
      let time h m s ms = maybe (throwError "Failed to create time")
                                return
                                (createTime h m s ms)
      case project r of
        Just (VRecord vr) ->
            liftM iVRecord $ either throwError return $ updateField f v vr
        Just (VDate d) -> do
            n <- extractInt v
            let (y, m, da) = dateToYearMonthDay d
            case parseDateField f of
              Just Year  -> liftM iVDate $ date (fromIntegral n) m da
              Just Month -> liftM iVDate $ date y n da
              Just Day   -> liftM iVDate $ date y m n
              _          -> evalDateError f
        Just (VTime t) -> do
            n <- extractInt v
            let (h, m, s, ms) = timeToHourMinSecMicroSec t
            case parseTimeField f of
              Just Hour        -> liftM iVTime $ time n m s ms
              Just Minute      -> liftM iVTime $ time h n s ms
              Just Second      -> liftM iVTime $ time h m n ms
              Just Microsecond -> liftM iVTime $ time h m s n
              _                -> evalTimeError f
        Just (VDateTime dt) -> do
            n <- extractInt v
            let (d, t) = dateTimeToDateTime dt
            let (y, m, da) = dateToYearMonthDay d
            let (h, mi, s, ms) = timeToHourMinSecMicroSec t
            let date' y m d = liftM (\d -> iVDateTime $ createDateTime d t)
                                    (date y m d)
            let time' h m s ms = liftM (iVDateTime . createDateTime d)
                                       (time h m s ms)
            case parseDateField f of
              Just Year  -> date' (fromIntegral n) m da
              Just Month -> date' y n da
              Just Day   -> date' y m n
              _ -> case parseTimeField f of
                     Just Hour        -> time' n mi s ms
                     Just Minute      -> time' h n s ms
                     Just Second      -> time' h mi n ms
                     Just Microsecond -> time' h mi s n
                     _                -> evalDateTimeError f
        _ -> evalRecordError
    evalAlg EBinOp{expBinOp = o, expBinOpArg1 = mv1, expBinOpArg2 = mv2} = do
      v1 <- mv1
      v2 <- mv2
      evalBinOp o v1 v2
          where evalBinOp :: BinOp -> Term v -> Term v -> EI env (Term v)
                evalBinOp o v1 v2 =
                    case o of
                      EQ ->
                          return $ iVBool $ v1 == v2
                      LEQ ->
                          case (project v1, project v2) of
                            (Just (VInt n1), Just (VInt n2)) ->
                                return $ iVBool $ n1 <= n2
                            (Just (VReal d1), Just (VReal d2)) ->
                                return $ iVBool $ d1 <= d2
                            (Just (VInt n1), Just (VReal d2)) ->
                                return $ iVBool $ fromIntegral n1 <= d2
                            (Just (VReal d1), Just (VInt n2)) ->
                                return $ iVBool $ d1 <= fromIntegral n2
                            (Just (VDuration vd1), Just (VDuration vd2)) ->
                                return $ iVBool $ vd1 <= vd2
                            _ -> evalBinOpError LEQ
                      PLUS ->
                          case (project v1, project v2) of
                            (Just (VInt n1), Just (VInt n2)) ->
                                return $ iVInt $ n1 + n2
                            (Just (VReal d1), Just (VReal d2)) ->
                                return $ iVReal $ d1 + d2
                            (Just (VInt n1), Just (VReal d2)) ->
                                return $ iVReal $ fromIntegral n1 + d2
                            (Just (VReal d1), Just (VInt n2)) ->
                                return $ iVReal $ d1 + fromIntegral n2
                            _ -> evalBinOpError PLUS
                      TIMES ->
                          case (project v1, project v2) of
                            (Just (VInt n1), Just (VInt n2)) ->
                                return $ iVInt $ n1 * n2
                            (Just (VReal d1), Just (VReal d2)) ->
                                return $ iVReal $ d1 * d2
                            (Just (VInt n1), Just (VReal d2)) ->
                                return $ iVReal $ fromIntegral n1 * d2
                            (Just (VReal _), Just (VInt _)) ->
                                evalBinOp TIMES v2 v1
                            _ -> evalBinOpError TIMES
                      DIV ->
                          case (project v1, project v2) of
                            -- First check for division by zero
                            (_, Just (VInt 0)) ->
                                evalBinOpError DIV
                            (_, Just (VReal 0.0)) ->
                                evalBinOpError DIV
                            (Just (VInt n1), Just (VInt n2)) ->
                                return $ iVInt $ n1 `div` n2
                            (Just (VReal d1), Just (VReal d2)) ->
                                return $ iVReal $ d1 / d2
                            (Just (VInt n1), Just (VReal d2)) ->
                                return $ iVReal $ fromIntegral n1 / d2
                            (Just (VReal d1), Just (VInt n2)) ->
                                return $ iVReal $ d1 / fromIntegral n2
                            _ -> evalBinOpError DIV
                      AND ->
                          case (project v1, project v2) of
                            (Just (VBool b1), Just (VBool b2)) ->
                                return $ iVBool $ b1 && b2
                            _ -> evalBinOpError AND
                      CONS ->
                          case project v2 of
                            Just (VList l) ->
                                return $ iVList $ v1 : l
                            _ -> evalListError
                      DPLUS ->
                          case (project v1, project v2) of
                            (Just (VDuration vd1), Just (VDuration vd2)) ->
                                return $ iVDuration $ normaliseDuration $
                                         vd1 `addDuration` vd2
                            _ -> evalBinOpError DPLUS
                      DTIMES ->
                          case (project v1, project v2) of
                            (Just (VInt n1), Just (VDuration vd2)) ->
                                return $ iVDuration $ normaliseDuration $
                                         scaleDuration n1 vd2
                            _ -> evalBinOpError DTIMES
    evalAlg EIfThenElse{expCondition = mcond,
                        expConditionThen = mthenBranch,
                        expConditionElse = melseBranch} = do
      cond <- mcond
      thenBranch <- mthenBranch
      elseBranch <- melseBranch
      case project cond of
        Just (VBool True)  -> return thenBranch
        Just (VBool False) -> return elseBranch
        _                  -> evalBoolError
    evalAlg ECase{expCase = mc, expCases = mcs} = do
      isSubType <- asks envSubTypeRel
      v <- mc
      case project v of
        Just (VRecord vr) ->
            -- Go through each case to find the first match
            case find (isSubType (vrecordName vr) . caseExpRecordName) mcs of
              Just CaseExp{caseExpVar = x,
                           caseExpBody = mc'} ->
                  -- Bind the case value and continue evaluation
                  local (\env -> env{envEvalEnv = Map.insert x v (envEvalEnv env)}) mc'
              Nothing ->
                  evalMatchError $ vrecordName vr
        _ -> evalRecordError


--------------------------------------------------------------------------------
-- Error messages
--------------------------------------------------------------------------------

evalFreeVarError x = throwError $ "Variable '" ++ x ++ "' is unbound"

evalBinOpError o =
    throwError $ "The operator '" ++ show o ++
                 "' is applied to wrong arguments"

evalExpectedError tp = "Expected a " ++ tp ++ " value"

evalMatchError rName =
    throwError $ "Pattern matching not exhaustive, no pattern for '" ++ rName ++
                 "'"

evalFunctionError :: EI env a
evalFunctionError = throwError $ evalExpectedError "function"

evalBoolError :: EI env a
evalBoolError = throwError $ evalExpectedError "boolean"

evalListError :: EI env a
evalListError = throwError $ evalExpectedError "list"

evalRecordError :: EI env a
evalRecordError = throwError $ evalExpectedError "record"

evalTimeError :: FieldName -> EI env a
evalTimeError f = throwError $ "Field '" ++ f ++ "' is not part of a time value"

evalDateError :: FieldName -> EI env a
evalDateError f = throwError $ "Field '" ++ f ++ "' is not part of a date value"

evalDateTimeError :: FieldName -> EI env a
evalDateTimeError f = throwError $ "Field '" ++ f ++ "' is not part of a datetime value"