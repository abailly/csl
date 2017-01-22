{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
  ScopedTypeVariables, TypeOperators, UndecidableInstances, TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Data.Value.TypeChecker
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved, Patrick Bahr, and Morten Ib Nielsen
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines type checking of values.
--
--------------------------------------------------------------------------------
module Poets.Data.Value.TypeChecker
    (
     typeCheckTerm
    )
    where

import Prelude hiding (foldl)
import Control.Monad.Error
import Control.Monad.Reader
import Data.List
import qualified Data.Map as Map
import Data.Comp
import Data.Comp.Derive
import Poets.Data.Value
import Poets.Data.Value.Utils
import Poets.Data.Type hiding (recordName)
import Poets.Data.Type.Utils
import Poets.Data.Type.Render

-- |The environment used in type checking consists of the expected type, the
-- record typing environment, and the entity typing environment.
data TCEnv t = TCEnv{
      expectedType :: Term t,
      recordEnv :: RecordEnv (Term t),
      entityEnv :: EntityTypingEnv
    }

-- |The type checker monad.
type TC t = ReaderT (TCEnv t) (Either String)

-- |Throw a type error.
typeCheckError :: String -> TC t a
typeCheckError = throwError

-- |Set the expected type.
withType :: (MonadReader (TCEnv t) m) => Term t -> m a -> m a
withType tp = local (\env -> env{expectedType = tp})

-- |Get the expected type.
getExpectedType :: (MonadReader (TCEnv t) m) => m (Term t)
getExpectedType = asks expectedType

-- |Get the record typing environment.
getRecordEnv :: (MonadReader (TCEnv t) m) => m (RecordEnv (Term t))
getRecordEnv = asks recordEnv

-- |Get the entity typing environment.
getEntityEnv :: (MonadReader (TCEnv t) m) => m EntityTypingEnv
getEntityEnv = asks entityEnv

-- |The type checker algebra.
class TypeChecker f t where
    typeCheckAlg :: Alg f (TC t ())

$(derive [liftSum] [''TypeChecker])

-- |Type check a term.
typeCheckTerm :: (Functor f, TypeChecker f t) =>
                 Term t -- ^The expected type.
              -> RecordEnv (Term t) -- ^The record typing environment.
              -> EntityTypingEnv -- ^The entity typing environment.
              -> Term f -- ^The term to type check.
              -> Either String ()
typeCheckTerm expType recordEnv entityEnv e =
    runReaderT (cata typeCheckAlg e) (TCEnv expType recordEnv entityEnv)

instance (TypeConstant :<: t,
          TypeList :<: t,
          TypeEnt :<: t,
          EqF t,
          Functor t,
          Render t) => TypeChecker Val t where
    typeCheckAlg (VInt _) =
        checkType TInt
    typeCheckAlg (VBool _) =
        checkType TBool
    typeCheckAlg (VString _) =
        checkType TString
    typeCheckAlg (VDate _) =
        checkType TDate
    typeCheckAlg (VTime _) =
        checkType TTime
    typeCheckAlg (VDateTime _) =
        checkType TDateTime
    typeCheckAlg VDuration{} =
        checkType TDuration
    typeCheckAlg (VReal _) =
        checkType TReal
    typeCheckAlg (VRecord vr) = do
      recordEnv <- getRecordEnv
      recInfo <- either typeCheckError
                        return
                        (getFullRecordInfo recordEnv $ vrecordName vr)
      -- First check that the record type is expected
      checkSubType' recordEnv (inject $ TRecord $ vrecordName vr)
      -- Then check that the record type is not abstract
      when (isAbstract recInfo)
           (typeCheckError $ "The record type '" ++ vrecordName vr ++ 
                             "' is abstract")
      -- Find record definition in typing environment
      fields <- either typeCheckError
                       return
                       (getTypeFields recordEnv $ vrecordName vr)
      -- Fields found in the record
      let fvs = fieldsMap' $ vrecordFields vr
      let fieldNamesFound = sort $ Map.keys fvs
      let fieldNamesExpected = fieldNamesSorted fields
      -- Report error if there is a mismatch between the expected fields
      -- and the fields found in the record value
      when (fieldNamesFound /= fieldNamesExpected)
           (let fieldNamesMissing = fieldNamesExpected \\ fieldNamesFound
                fieldNamesWrong = fieldNamesFound \\ fieldNamesExpected in
            typeCheckError $
                  (if not $ null fieldNamesMissing then
                       "The field(s) " ++ showFields fieldNamesMissing ++
                       " are missing"
                   else "") ++
                  (if not $ null fieldNamesWrong then
                      (if not $ null fieldNamesMissing then ". " else "") ++
                       "The field(s) " ++ showFields fieldNamesWrong ++
                       " were not expected"
                   else ""))
      -- Function for type checking a field in a record
      let checkField (f,tp) = do
            field <- either (\_ -> typeCheckError $
                                   "Field '" ++ f ++
                                   "' is undefined for record '" ++
                                   vrecordName vr ++ "'")
                            return
                            (getFieldInfo fields f)
            -- Check that the type of the field value is a sub type of the
            -- expected type
            withType (deepInject $ fieldType field) tp
      -- Check that each field type is correct
      mapM_ checkField $ Map.toList fvs
          where showFields = intercalate ", " . map (\x -> "'" ++ x ++ "'")
    typeCheckAlg (VEnt VEntity{ventType = rName, ventId = id}) = do
      reqType <- getExpectedType
      case project reqType of
        Just (TEnt tp) -> do
          recordEnv <- getRecordEnv
          entityEnv <- getEntityEnv
          -- Check that the entity type is a sub type of the expected type
          withType tp $ checkSubType' recordEnv (iTRecord rName)
          -- Then check that the entity typing is right
          either typeCheckError return $ checkEntityTyping recordEnv entityEnv id rName
        _ ->
            typeCheckError $ "Expected type '" ++ show (renderTerm reqType) ++
                             "' but found a value of entity type"
    typeCheckAlg (VList l) = do
      reqType <- getExpectedType
      case project reqType of
        Just (TList tp) ->
            mapM_ (withType tp) l
        _ ->
            typeCheckError $ "Expected type '" ++ show (renderTerm reqType) ++
                             "' but found a value of list type"

-- Check that the supplied type equals the expected type from the typing monad
checkType tp = do
  reqType <- getExpectedType
  when (inject tp /= reqType)
       (typeCheckError $ "Expected type '" ++ show (renderTerm reqType) ++
                         "' but found type '" ++
                         show (renderTerm $ inject tp) ++ "'")

-- Check that the supplied type is a sub type of the expected type from the
-- typing monad
checkSubType' recordEnv tp = do
  reqType <- getExpectedType
  either typeCheckError return $ checkSubType recordEnv tp reqType