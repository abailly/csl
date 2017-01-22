{-# LANGUAGE FlexibleContexts, DoAndIfThenElse, TypeOperators #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Data.Type.Utils
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved, Patrick Bahr, and Morten Ib Nielsen
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module contains utility functions for the AST defined in
-- 'Poets.Data.Type'.
--
--------------------------------------------------------------------------------

module Poets.Data.Type.Utils
    (
     checkSubType,
     findCommonSuperTypes,
     getSubTypes,
     getSuperTypes,
     getSuperTypes',
     getTypeField,
     getTypeFields,
     getFullRecordInfo,
     isSubType,
     isAbstract,
     isLocked,
     isHidden,
     remRecordInfos,
     validateRecordEnv,
     getFieldNames,
     undefinedRecord,
     checkEntityTyping,
     uncovered
    ) where

import Prelude hiding (foldl, sequence_)
import Control.Monad.Error hiding (msum, sequence_)
import Data.List (intersperse, delete)
import Data.Foldable hiding (concat, mapM_, elem)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Comp
import Data.Graph
import Poets.Data.Render
import Poets.Data.Value (RecordName, FieldName, EntId)
import Poets.Data.Type

-- |Check that @tp1@ is a sub type of @tp2@. If both are record types,
-- this amounts to record sub typing. Otherwise it amounts to equality of
-- @tp1@ and @tp2@.
checkSubType :: (EqF t, TypeConstant :<: t, Functor t, Render t) =>
                RecordEnv (Term t) -- ^The record environment. 
             -> Term t -- ^@tp1@
             -> Term t -- ^@tp2@
             -> Either String ()
checkSubType recordEnv tp1 tp2 =
    case (project tp1, project tp2) of
      (Just (TRecord tp1'), Just (TRecord tp2')) ->
          -- Both tp1 and tp2 are record types, so check that tp1 is a sub type
          -- of tp2
          case isSubType recordEnv tp1' tp2' of
            Left err    -> Left err
            Right False -> subTypeError tp1 tp2
            Right True  -> return ()
      _ ->
          when (tp1 /= tp2) (subTypeError tp1 tp2)
    where subTypeError tp1 tp2 =
              Left $ "Expected a sub type of '" ++ show (renderTerm tp2) ++
                     "' but found type '" ++ show (renderTerm tp1) ++ "'"

-- |This function removes the given record name from the record environment.
remRecordInfos :: RecordEnv e    -- ^The record environment.  
               -> [RecordName] -- ^A record name.
               -> RecordEnv e    -- ^The new record environment.  
remRecordInfos = foldl remRecordInfo

-- |The function 'getTypeFields' /requires/ that type definitions are acyclic.
-- Otherwise it might loop indefinitely
getTypeFields :: RecordEnv e -- ^The record environment.
              -> RecordName -- ^A record name.
              -> Either String (FieldEnv e) -- ^The field record environment for the record of the given name or an error message.
getTypeFields recordEnv rn = do
  r <- getRecordInfo recordEnv rn
  let localFields = recordFields r
  foldM (\fenv rname' -> do fenv' <- getTypeFields recordEnv rname'
                            return $ combFieldEnvs fenv' fenv)
        localFields (Set.toList $ recordExtends r)

{-| This function returns the record definition with the given name in
the given record environment. If there is no such record defined, then
the monadic computation 'fail's. Inherited fields are included. -}
getFullRecordInfo :: RecordEnv e -- ^The record environment. 
                  -> RecordName -- ^A record name.
                  -> Either String (Record e) -- ^Information about the record with the given name.
getFullRecordInfo recordEnv rn = do
  recInfo <- getRecordInfo recordEnv rn
  fieldEnv <- getTypeFields recordEnv rn
  return recInfo{recordFields = fieldEnv}

-- |Get the set of field names for the given record type.
getFieldNames :: RecordEnv e -- ^The record environment.
              -> RecordName -- ^A record name.
              -> Either String (Set.Set FieldName)
getFieldNames env name =  liftM fieldNamesSet $ getTypeFields env name

-- |The function 'getTypeField' /requires/ that type definitions are acyclic.
getTypeField :: RecordEnv e -- ^The record environment.
             -> RecordName -- ^A record name.
             -> FieldName -- ^A field name.
             -> Either String (Field e) -- ^The field with the given field name in the record of the given name.
getTypeField recordEnv rn fn = do
  fieldEnv <- getTypeFields recordEnv rn
  case getFieldInfo fieldEnv fn of
    Left _ -> Left $ "There is no field named " ++ fn ++
                     " in the record named " ++ rn
    Right f -> return f

-- |Return the set of all (proper) subtypes for a given record name
getSubTypes' :: Eq e => RecordEnv e
             -> RecordName
             -> Either String [RecordName]
getSubTypes' recordEnv rn2 = do
  let recNames = getRecordNames' recordEnv
  rNames <- filterM (\rn1 -> isSubType recordEnv rn1 rn2) recNames
  return $ delete rn2 rNames

-- |Return the set of all (proper) subtypes for a given record name
getSubTypes :: Eq e => RecordEnv e
            -> RecordName
            -> Either String (Set.Set RecordName)
getSubTypes recordEnv rn2 = liftM Set.fromList $ getSubTypes' recordEnv rn2

getSuperTypes' :: RecordEnv e -> RecordName -> Either String [RecordName]
getSuperTypes' recordEnv rName = getSuperTypes' rName
    where getSuperTypes' rName = do
            record <- getRecordInfo recordEnv rName
            foldM (\supers rName -> do supers' <- getSuperTypes' rName
                                       return $ supers' ++ supers)
                  [rName] (Set.toList $ recordExtends record)

getSuperTypes :: RecordEnv e -> RecordName -> Either String (Set RecordName)
getSuperTypes env name = liftM Set.fromList $ getSuperTypes' env name
          
-- |The function 'isSubType' /requires/ that type definitions are acyclic.
-- Otherwise it might loop indefinitely.
isSubType :: Eq e => RecordEnv e -- ^The record environment.  
          -> RecordName -- ^A record name, @rn1@.
          -> RecordName -- ^A record name, @rn2@.
          -> Either String Bool
isSubType recordEnv rn1 rn2 = 
    if rn1 == rn2 then return True
    else do
      record1 <- getRecordInfo recordEnv rn1
      record2 <- getRecordInfo recordEnv rn2
      foldM (\b rName -> do b' <- isSubType recordEnv rName rn2
                            return $ b || b')
            False
            (Set.toList $ recordExtends record1)

-- |Check whether a record type is abstract.
isAbstract :: Record e -> Bool
isAbstract rInfo = Set.member RecordIsAbstract (recordAttributes rInfo)

-- |Check whether a record type is locked for changes.
isLocked :: Record e -> Bool
isLocked rInfo = Set.member RecordIsLocked (recordAttributes rInfo)

-- |Check whether a record type is hidden.
isHidden :: Record e -> Bool
isHidden rInfo = Set.member RecordIsHidden (recordAttributes rInfo)

-- |Get a list of all non-abstract descendants of the supplied record type.
nonAbstractDescendants :: Eq e => RecordEnv e
                       -> RecordName
                       -> Either String [RecordName]
nonAbstractDescendants recordEnv rName = do
  rInfo <- getRecordInfo recordEnv rName
  (if isAbstract rInfo then do
       rNames <- getSubTypes' recordEnv rName
       rNames' <- mapM (nonAbstractDescendants recordEnv) rNames
       return $ concat rNames'
   else
       return [rName])

-- |Given a record name, @rName@, a list of record names, @rNames@, calculate a
-- set of non-abstract records, which are descendants of @rName@ and are not sub
-- type of any of the records in @rNames@.
uncovered :: Eq e => RecordEnv e
          -> RecordName
          -> [RecordName]
          -> Either String [RecordName]
uncovered recordEnv rName rNames = do
  let recordEnv' = remRecordInfos recordEnv rNames
  (if isDefined recordEnv' rName then
       nonAbstractDescendants recordEnv' rName
   else
       return [])

-- |Returns the least (w.r.t. subtyping) common super types of the two specified
-- record types (if it exists).
findCommonSuperTypes :: Eq e => RecordEnv e
                     -> RecordName
                     -> RecordName
                     -> Either String [RecordName]
findCommonSuperTypes recordEnv rn1 rn2 = do
  b1 <- isSubType recordEnv rn1 rn2
  b2 <- isSubType recordEnv rn2 rn1
  if b1 then
      return [rn2]
  else
      if b2 then
          return [rn1]
      else
          do record1 <- getRecordInfo recordEnv rn1
             foldM (\supers rName -> do supers' <- findCommonSuperTypes recordEnv rName rn2
                                        return $ supers ++ supers')
                   [] (Set.toList $ recordExtends record1)

{-| This function checks whether the given type contains a record type
that is not defined in the given record environment. If that is the
case, a witness is returned. -}
undefinedRecord :: (TypeConstant :<: f, Foldable f, Functor f)
                => RecordEnv g -> Term f -> Maybe RecordName
undefinedRecord env ty = cata run ty
    where run :: (TypeConstant :<: f, Foldable f) => f (Maybe RecordName) -> Maybe RecordName
          run ty = case proj ty  of
                           Just (TRecord name) -> if isDefined env name
                                                  then Nothing
                                                  else Just name
                           _ -> msum ty

-- |Check that the entity typing environment contains the given entity and that
-- it has a type which is a sub type of the expected type.
checkEntityTyping :: Eq e => RecordEnv e
                  -> EntityTypingEnv
                  -> EntId
                  -> RecordName
                  -> Either String ()
checkEntityTyping recordEnv entityEnv id rName =
    -- Then check that the entity typing is right
    case Map.lookup id entityEnv of
      Just rName' -> do
        b <- isSubType recordEnv rName' rName
        unless b (Left $ "Expected type '" ++ rName ++
                         "' for entity '" ++ show id ++
                         "' but found type '" ++ rName' ++ "'")
      Nothing ->
          Left $ "Entity '" ++ show id ++ "' not found"

--------------------------------------------------------------------------------
-- Validation of record environments
--------------------------------------------------------------------------------

-- |Check whether a record typing environment is well-formed. That is:
--
-- 1. Record names are unique.
--
-- 2. All super type records are defined when extending a record.
--
-- 3. There is no cyclic inheritance.
--
-- 4. Field names in a record are unique.
--
-- 5. Field types are defined, e.g. if field f has type A, then A must be
-- defined.
--
-- 6. The record types used in the field of a record definition is defined, and
-- it is not a subtype of the defining record (i.e., no infinite data
-- structures).
validateRecordEnv :: (Functor t, Foldable t, EqF t, TypeConstant :<: t)
                     => RecordEnv (Term t) -> Either String ()
validateRecordEnv recordEnv = do
  let records = recordEnvList recordEnv
  -- Throws an error if recordEnv contains a record with multiple definitions.
  uniqueRecordNames records
  -- Throws an error if any of the records extend an undefined record.
  mapM_ superRecordIsDefined records
  -- Throws an error if recordEnv is not acyclic.
  acyclicInheritance records
  -- Throws an error if any of the records define a field multiple times.
  mapM_ uniqueFieldNames records
  -- Throws an error if any of the field types are undefined.
  mapM_ (\r -> mapM_ (fieldTypeDefined (recordName r))
                     (fieldEnvList $ recordFields r))
        records
  -- Throws an error if the record typing environment contains a recursive
  -- definition.
  noRecursion records
  -- Success (nothing has failed)
  return ()
      where uniqueRecordNames :: [Record a] -> Either String ()
            uniqueRecordNames records = 
                let recordNames = map recordName records in
                let dups = [r | r <- recordNames,
                                length (filter ((==) r) recordNames) > 1] in
                if null dups then
                    return ()
                else
                    Left $ "record type '" ++ head dups ++ "' is defined twice"
            superRecordIsDefined :: Record (Term t) -> Either String ()
            superRecordIsDefined r =
                mapM_ (checkRn r) (Set.toList $ recordExtends r)
            checkRn r rn =
                unless (isDefined recordEnv rn)
                       (Left $ "record type " ++ rn ++
                               ", which the record type " ++ recordName r ++
                               " is supposed to extend, is not defined.")
            acyclicInheritance :: [Record e] -> Either String ()
            acyclicInheritance records = do
                -- Inheritance graph
                let inhGraph = [(r, recordName r, Set.toList $ recordExtends r) | r <- records]

                let sccs = [scc | CyclicSCC scc <- stronglyConnComp inhGraph]
                case sccs of
                  scc : _ ->
                      Left $ "cyclic inheritance; " ++
                             concat (intersperse ", " $ map recordName scc)
                  [] ->
                      return ()
            uniqueFieldNames :: Record e -> Either String ()
            uniqueFieldNames r = do
                let rn = recordName r 
                fieldEnv <- getTypeFields recordEnv rn
                let fs = map fieldName (fieldEnvList fieldEnv)
                let dups = [f | f <- fs, length (filter ((==) f) fs) > 1]
                if null dups then
                    return ()
                else
                    Left $ "record type '" ++ rn ++
                           "' contains multiple definitions of the field '" ++
                           head dups ++ "'"
            fieldTypeDefined :: (Functor t, Foldable t, TypeConstant :<: t) =>
                                RecordName -> Field (Term t) -> Either String ()
            fieldTypeDefined rn f =
                cata (checkType rn (fieldName f)) (fieldType f)
            checkType rn fn ty =
                case proj ty of
                  Just (TRecord rn') ->
                      if isDefined recordEnv rn' then
                          return ()
                      else
                          Left $ "record type '" ++ rn ++
                                 "' defines the field '" ++ fn ++
                                 "' whose type is referring to the " ++
                                 "undefined record type '" ++ rn' ++ "'"
                  _ -> sequence_ ty
            noRecursion :: (Functor t, Foldable t, TypeConstant :<: t) =>
                           [Record (Term t)] -> Either String ()
            noRecursion records = do
                -- Record dependency graph
                let depGraph = [(r, recordName r, deps) |
                                r <- records,
                                let deps = [r' |
                                            r' <- map recordName records,
                                            f <- fieldEnvList $ recordFields r,
                                            r' `elem` cata recDeps (fieldType f)
                                           ]
                               ]
                let sccs = [scc | CyclicSCC scc <- stronglyConnComp depGraph]
                case sccs of
                   scc : _ -> Left $ "cyclic dependency; " ++
                               concat (intersperse ", " $ map recordName scc)
                   [] ->  return ()
                return ()
            recDeps ty = case proj ty of Just (TRecord r) -> [r]
                                         _                -> foldl (++) [] ty