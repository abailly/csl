{-# LANGUAGE FlexibleContexts, TypeOperators #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Contracts.Language.CSL.BuiltIn
-- Copyright   :  3gERP, 2012
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines the built-in functions of CSL, that is their types and
-- their implementation.
--
--------------------------------------------------------------------------------

module Poets.Contracts.Language.CSL.BuiltIn
    (
     builtIn
    ) where

import Poets.Data
import Poets.Contracts.Language.CSL.AST.Type
import Poets.Contracts.Language.CSL.AST.Exp
import Poets.Contracts.Language.CSL.Typing.TypeInferer
import Data.Foldable
import Control.Monad.Error

-- |The types and implementations of the built-in functions. Each function name
-- is associated with its type, any typing constraints that might pertain to its
-- usage, and associated value.
builtIn :: (SubTypeConstraint :<: c,
            EqConstraint :<: c,
            TypeConstant :<: t,
            TypeList :<: t,
            TypeVar :<: t,
            TypeFunction :<: t,
            Val :<: v,
            VFun :<: v,
            EqF v) => [(String, (Term t, [c (Term t)]), Term v)]
builtIn = [("foldr",
            ((a ~> b ~> b) ~> b ~> iTList a ~> b, []),
            iVFun $ \fun ->
                return $ iVFun $ \init ->
                    return $ iVFun $ \list -> do
                      f <- projFun fun
                      xs <- projList list
                      foldrM (\x a -> do f' <- f x
                                         f'' <- projFun f'
                                         f'' a) init xs),
          ("ceil",
           (a ~> iTInt, [inj $ SubTypeConstraint a iTReal]),
           iVFun $ \val ->
               case project val of
                 Just (VReal v) -> return $ iVInt $ ceiling v
                 Just (VInt v)  -> return $ iVInt v
                 _              -> evalNumericError),
          ("subtractDate",
           (iTDateTime ~> iTDateTime ~> iTDuration, []),
           iVFun $ \d1 ->
               return $ iVFun $ \d2 -> do
                 dt1 <- projDateTime d1
                 dt2 <- projDateTime d2
                 return $ iVDuration $ dt1 `dateTimeDiff` dt2)]
    where a = iTVar "a"
          b = iTVar "b"
          projList l = case project l of
                         Just (VList xs) -> return xs
                         _               -> evalListError
          projFun f = case project f of
                        Just (VFun f') -> return f'
                        _              -> evalFunctionError
          projDateTime d = case project d of
                             Just (VDateTime dt) -> return dt
                             _                   -> evalDateTimeError
          evalExpectedError tp = "Expected a " ++ tp ++ " value"
          evalFunctionError = throwError $ evalExpectedError "function"
          evalNumericError = throwError $ evalExpectedError "numeric"
          evalListError = throwError $ evalExpectedError "list"
          evalDateTimeError = throwError $ evalExpectedError "datetime"