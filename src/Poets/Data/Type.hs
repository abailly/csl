{-# LANGUAGE TemplateHaskell, TypeOperators, FlexibleContexts #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Data.Type
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved, Patrick Bahr, and Morten Ib Nielsen
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines the AST for POETS types and POETS record typing
-- environments.
--
--------------------------------------------------------------------------------

module Poets.Data.Type
    (
     TypeConstant(..),
     TypeEnt(..),
     TypeList(..),
     Type,
     Typ,
     Field(..),
     FieldAttribute(..),
     FieldEnv,
     POETSFieldEnv,
     Record(..),
     RecordAttribute(..),
     RecordEnv,
     POETSRecordEnv,
     EntityTypingEnv,
     emptyEntityTypingEnv,
     newRecordEnv,
     newRecordEnvMap,
     recordEnvList,
     recordEnvMap,
     recordEnvSize,
     getFields,
     remRecordInfo,
     addRecordInfo,
     addRecordInfos,
     extendRecord,
     isEmpty,
     getRecordNames',
     getRecordNames,
     isDefined,
     getRecordInfo,
     addRecordAttribute,
     newFieldEnv,
     fieldNamesSorted,
     fieldNames,
     fieldNamesSet,
     emptyFieldEnv,
     combFieldEnvs,
     addFieldInfo,
     fieldEnvSorted,
     fieldEnvList,
     getFieldInfo,
     fieldEnvMap,
     addFieldAttribute,
     -- * Smart Constructors
     iTRecord,
     iTReal,
     iTBool,
     iTDuration,
     iTList,
     iTString,
     iTDate,
     iTTime,
     iTDateTime,
     iTInt,
     iTEnt
    ) where

import Prelude hiding (foldl, sequence_)
import Control.Monad.Error hiding (msum, sequence_)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Comp
import Data.Comp.Derive
import Poets.Data.Value (FieldName, RecordName, EntId)

--------------------------------------------------------------------------------
-- Type definitions
--------------------------------------------------------------------------------

-- |The signature for type constants.
data TypeConstant e = TInt -- ^Integer type.
                    | TBool -- ^Boolean type.
                    | TString -- ^String type.
                    | TDate -- ^Date type.
                    | TTime -- ^Time type.
                    | TDateTime -- ^DateTime type.
                    | TDuration -- ^Duration type.
                    | TReal -- ^Real type.
                    | TRecord RecordName -- ^Record type.
                      deriving (Eq, Ord)

-- |The signature for list types.
data TypeList e = TList e -- ^List type.
                  deriving (Eq, Ord)

-- |The signature for entity types.
data TypeEnt e = TEnt e -- ^Entity type.
                 deriving (Eq, Ord)

-- |The combined signature for POETS types.
type Typ = TypeConstant :+: TypeList :+: TypeEnt

-- |POETS types.
type Type = Term Typ

$(derive [makeFunctor, makeFoldable, makeTraversable, makeEqF,
          makeOrdF,smartConstructors]
         [''TypeConstant, ''TypeList, ''TypeEnt] )


--------------------------------------------------------------------------------
-- Typing environment definitions
--------------------------------------------------------------------------------

-- |The definition of a field type.
data Field t = Field {
      fieldName :: FieldName, -- ^The name of the field type.
      fieldType :: t, -- ^The type of the field.
      fieldAttributes :: Set FieldAttribute  -- ^Field attributes.
    } deriving (Ord, Eq, Show)

-- |The possible attributes of a field.
data FieldAttribute = FieldOrder Int
                    | FieldRestriction String
                      deriving (Ord, Show)

instance Eq FieldAttribute where
    (==) (FieldOrder _) (FieldOrder _) = True
    (==) (FieldRestriction r1) (FieldRestriction r2) = r1 == r2
    (==) _ _ = False

-- |A field typing environment is a mapping of field names to their type
-- definition.
data FieldEnv t = FieldEnv (Map FieldName (Field t))
                  deriving (Show, Eq, Ord)

-- |The definition of a record type.
data Record t = Record {
      recordName :: RecordName, -- ^The name of the record type.
      recordFields :: FieldEnv t, -- ^The field definitions.
      recordExtends :: Set RecordName, -- ^A record type may extend existing record types, thereby inheriting all field definitions.
      recordAttributes :: Set RecordAttribute
    } deriving(Eq, Show, Ord)

-- |The possible attributes of a record definition.
data RecordAttribute = RecordIsAbstract
                     | RecordIsLocked
                     | RecordIsHidden
                       deriving (Ord, Show)

instance Eq RecordAttribute where
    (==) RecordIsAbstract RecordIsAbstract = True
    (==) RecordIsLocked RecordIsLocked = True
    (==) RecordIsHidden RecordIsHidden = True
    (==) _ _ = False

-- |A record typing environment is a mapping of record names to their type
-- definition.
data RecordEnv t = RecordEnv (Map RecordName (Record t))
                   deriving (Show, Eq, Ord)

$(derive [makeFunctor, makeFoldable, makeTraversable]
         [''Field, ''FieldEnv, ''Record, ''RecordEnv] )

-- |POETS record typing environment.
type POETSRecordEnv = RecordEnv Type

-- |POETS field typing environment.
type POETSFieldEnv = FieldEnv Type

-- |Typing environment for entities.
type EntityTypingEnv = Map EntId RecordName

-- |Empty entity typing environment.
emptyEntityTypingEnv = Map.empty


--------------------------------------------------------------------------------
-- abstract interface to RecordEnv
--------------------------------------------------------------------------------

newRecordEnv :: [Record e] -> RecordEnv e
newRecordEnv records = RecordEnv $ Map.fromList (map (\r->(recordName r,r)) records)

newRecordEnvMap :: Map RecordName (Record e) -> RecordEnv e
newRecordEnvMap = RecordEnv

-- | Obtains the fields defined by the given record including fields defined by
-- super types.
getFields :: RecordEnv e -> RecordName -> Either String (FieldEnv e)
getFields renv rname = liftM FieldEnv $ collect rname
    where collect rname = do
            rec <- getRecordInfo renv rname
            let (FieldEnv fenv) = recordFields rec
            foldM (\fenv rname' -> do fenv' <- collect rname'
                                      return $ Map.union fenv' fenv)
                  fenv (Set.toList $ recordExtends rec)

{-| This function add the given field to the given record.  -}
extendRecord :: RecordEnv e -> RecordName -> Field e -> Either String (RecordEnv e)
extendRecord renv@(RecordEnv env) rname field@Field{fieldName = fname} = do
    check
    return $ RecordEnv $ Map.adjust change rname env
    where change r = r {recordFields = change' $ recordFields r}
          change' (FieldEnv fenv) = FieldEnv $ Map.insert fname field fenv
          check = do 
            record <- getRecordInfo renv rname
            case getFieldInfo (recordFields record) fname of
              Left _ -> return ()
              Right _ -> Left $ "field " ++ fname ++ " is already defined in record " ++ rname

-- |Extract a list of record definitions from a record environment.
recordEnvList :: RecordEnv e -> [Record e]
recordEnvList (RecordEnv e) = Map.elems e

-- |Extract a list of record definitions from a record environment.
recordEnvMap :: RecordEnv e -> Map RecordName (Record e)
recordEnvMap (RecordEnv e) = e


recordEnvSize :: RecordEnv t -> Int
recordEnvSize (RecordEnv e) = Map.size e

{-|
  This function removes the given record name from the record environment.
-}
remRecordInfo :: RecordEnv e  -- ^The record environment.  
              -> RecordName -- ^A record name.
              -> RecordEnv e  -- ^The new record environment.  
remRecordInfo (RecordEnv env) name = RecordEnv $ Map.delete name env

{-| This function adds the given list of records to the given record
environment. If a record of the same name is present, it is
overwritten.  -}
addRecordInfo :: RecordEnv e -- ^The record environment.  
              -> Record e  -- ^A record.
              -> RecordEnv e -- ^The new record environment.  
addRecordInfo (RecordEnv env) record = RecordEnv $ Map.insert (recordName record) record env

addRecordInfos :: RecordEnv e -- ^The record environment.  
              -> [Record e]  -- ^A record.
              -> RecordEnv e -- ^The new record environment.  
addRecordInfos (RecordEnv env) recs = RecordEnv $ recMap `Map.union` env
    where recMap = Map.fromList $ map (\r -> (recordName r,r)) recs


{-|
  This function returns True iff the given record environment is empty.
-}
isEmpty :: RecordEnv e -> Bool
isEmpty (RecordEnv env) = Map.null env



getRecordNames' :: RecordEnv e -> [RecordName]
getRecordNames' (RecordEnv env) = Map.keys env

getRecordNames :: RecordEnv e -> Set RecordName
getRecordNames (RecordEnv env) = Map.keysSet env


{-| This function returns true iff the record with the given name is
defined in the record environment.  -}

isDefined :: RecordEnv e -- ^The record environment.  
          -> RecordName -- ^A record name.
          -> Bool -- ^True iff the record with the given name is defined in the record environment.
isDefined (RecordEnv env) rn = Map.member rn env

{-| This function returns the record definition with the given name in
the given record environment. If there is no such record defined, then
the monadic computation 'fail's. Inherited fields are /not/ included. -}
getRecordInfo :: RecordEnv e -- ^The record environment. 
              -> RecordName -- ^A record name.
              -> Either String (Record e) -- ^Information about the record with the given name.
getRecordInfo (RecordEnv te) rn =
    case Map.lookup rn te of
      Nothing -> Left $ "The typing environment has no record named " ++ rn
      Just r -> return r

addRecordAttribute :: Record e -> RecordAttribute -> Record e
addRecordAttribute r a = r{recordAttributes = Set.insert a (recordAttributes r)}

-------------------------------------
-- abstract interface to FieldEnv --
-------------------------------------

-- |Constructor for field environments.
newFieldEnv :: [Field e] -> FieldEnv e
newFieldEnv fields = FieldEnv $ Map.fromList (map (\f->(fieldName f,f)) fields)

{-| This function provides the list of field names defined in the
given field environment sorted in ascending order -}

fieldNamesSorted :: FieldEnv t -> [FieldName]
fieldNamesSorted (FieldEnv e) = Map.keys e

{-| This function provides the list of field names defined in the
  given field environment. -}
fieldNames :: FieldEnv t -> [FieldName]
fieldNames (FieldEnv e) = Map.keys e

-- |This function provides the set of field names defined in the given field
-- environment.
fieldNamesSet :: FieldEnv t -> Set FieldName
fieldNamesSet (FieldEnv e) = Map.keysSet e

-- |The empty field environment.
emptyFieldEnv :: FieldEnv e
emptyFieldEnv = FieldEnv Map.empty


{-| This function combines two field environments.  -}
combFieldEnvs :: FieldEnv e -> FieldEnv e -> FieldEnv e
combFieldEnvs (FieldEnv e1) (FieldEnv e2) = FieldEnv $ Map.union e1 e2

addFieldInfo :: FieldEnv e -> Field e -> FieldEnv e
addFieldInfo (FieldEnv e) f = FieldEnv $ Map.insert (fieldName f) f e

{-| This functions returns the list of all fields defined in the given
field environment sorted by their names in ascending order.  -}

fieldEnvSorted :: FieldEnv t -> [Field t]
fieldEnvSorted (FieldEnv e) = Map.elems e

{-| This functions returns the list of all fields defined in the given
field environment.  -}

fieldEnvList :: FieldEnv t -> [Field t]
fieldEnvList (FieldEnv e) = Map.elems e

fieldEnvMap :: FieldEnv t -> Map FieldName (Field t)
fieldEnvMap (FieldEnv e) = e


-- |Get the type definition of a field for a field of the supplied name.
getFieldInfo :: FieldEnv e -- ^The field typing environment.
             -> FieldName -- ^A field name.
             -> Either String (Field e) -- ^Information about the field with the given name (if it exists).
getFieldInfo (FieldEnv t) name = case Map.lookup name t of
                                   Nothing -> Left $ "field " ++ name ++ " is not defined"
                                   Just field -> return field


addFieldAttribute :: Field e -> FieldAttribute -> Field e
addFieldAttribute f a = f{fieldAttributes = Set.insert a (fieldAttributes f)}
