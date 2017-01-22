{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.Data.Value
-- Copyright   :  3gERP, 2010
-- License     :  AllRightsReserved
-- Maintainer  :  Tom Hvitved, Patrick Bahr, and Morten Ib Nielsen
-- Stability   :  unknown
-- Portability :  unknown
--
-- This module defines the AST for values.
--
--------------------------------------------------------------------------------

module Poets.Data.Value
    (
     Date,
     Time,
     DateTime,
     RecordName,
     FieldName,
     VRecord(..),
     VField(..),
     VFields,
     EntId,
     VEntity(..),
     Val(..),
     Value,
     module Poets.Data.Value.Duration,
     lookupField,
     fieldsList,
     fieldsSorted,
     fieldsNull,
     fieldsMap,
     addFieldValue,
     updateFieldValue,
     setFieldValue,
     newFields,
     -- * Smart Constructors
     iVBool,
     iVString,
     iVDate,
     iVTime,
     iVDateTime,
     iVDuration,
     iVReal,
     iVRecord,
     iVEnt,
     iVList,
     iVInt,
     -- * Smart Constructors w/ annotations
     iAVBool,
     iAVString,
     iAVDate,
     iAVTime,
     iAVDateTime,
     iAVDuration,
     iAVReal,
     iAVRecord,
     iAVEnt,
     iAVList,
     iAVInt
    ) where

import Poets.Data.Value.Duration
import Control.Monad.Error
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (LocalTime, TimeOfDay)
import Data.Comp
import Data.Comp.Derive
import Prelude hiding (and)
import Data.Map (Map)
import qualified Data.Map as Map

-- |The name of a record.
type RecordName = String

-- |The name of a field in a record.
type FieldName = String

-- |The internal representation of entities.
type EntId = Int

-- |A set of record fields.
data VFields e = VFields (Map FieldName (VField e))
                 deriving (Eq, Ord)

-- |A record.
data VRecord e = VR{
      vrecordName :: RecordName, -- ^The name (type) of the record.
      vrecordFields :: VFields e -- ^The fields of the record.
    } deriving (Eq, Ord)

-- |An entity.
data VEntity = VEntity {
      ventType :: RecordName, -- ^The name (type) of the entity.
      ventId :: EntId, -- ^The entity ID.
      ventContext :: Maybe DateTime -- ^The temporal context.
    } deriving (Ord)

instance Eq VEntity where
    vent1 == vent2 = ventId vent1 == ventId vent2

-- |A record field.
data VField e = VF{
      vfieldName :: FieldName, -- ^The name of the field.
      vfieldValue :: e -- ^The value of the field.
    } deriving (Eq, Ord)

-- |A date.
type Date = Day

-- |A time of day.
type Time = TimeOfDay

-- |A date and time of day.
type DateTime = LocalTime

-- |The signature for values.
data Val e = VInt Int -- ^Integer value.
           | VBool Bool -- ^Boolean value.
           | VString String -- ^String value.
           | VDate Date -- ^A UTC date value.
           | VTime Time -- ^A UTC time value.
           | VDateTime DateTime -- ^A UTC date+time value.
           | VDuration (VDuration Int) -- ^Duration value.
           | VReal Double -- ^Real value.
           | VRecord (VRecord e) -- ^Record value.
           | VEnt VEntity -- ^Entity value.
           | VList [e] -- ^List value.
             deriving (Eq)

-- |POETS values.
type Value = Term Val

$(derive [makeFunctor, makeFoldable, makeTraversable]
         [''VFields, ''VField, ''VRecord] )
$(derive [makeFunctor, makeFoldable, makeTraversable,
          makeEqF, makeOrdF, smartConstructors, smartAConstructors]
         [''Val] )


-----------------------------------
-- abstract interface to VFields --
-----------------------------------

fieldsMap :: VFields e -> Map FieldName (VField e)
fieldsMap (VFields fs) = fs

fieldsList :: VFields e -> [VField e]
fieldsList (VFields fs) = Map.elems fs

fieldsSorted :: VFields e -> [VField e]
fieldsSorted (VFields fs) = Map.elems fs


{-| This function constructs a new collection of fields from the given
list of fields. -}

newFields :: [VField e] -> VFields e
newFields = VFields . Map.fromList . map (\f-> (vfieldName f, f)) 


fieldsNull :: VFields e -> Bool
fieldsNull (VFields fs) = Map.null fs

lookupField' :: FieldName -- ^Field name.
             -> VFields e -- ^fields
             -> Either String (VField e)
lookupField' fname (VFields fs) =
    case Map.lookup fname fs of
      Just vf -> return vf
      Nothing -> Left $ "field '" ++ fname ++ "' is not defined"

-- |Extract the value of a field in a value record.
lookupField :: FieldName -- ^Field name.
            -> VRecord e -- ^Value record.
            -> Either String e 
lookupField fn VR{vrecordFields = fields, vrecordName = rname} =
    case lookupField' fn fields  of
      Right vf -> return $ vfieldValue vf
      Left _ -> Left $ "field '" ++ fn ++
                 "' is not defined for record '" ++ rname ++ "'"

-- |Add a new field to a record value. Fails if the field already exists.
addFieldValue :: FieldName -> e -> VFields e -> Either String (VFields e)
addFieldValue fname val (VFields fs) =
    case Map.lookup fname fs of
      Just _ -> Left $ "field '" ++ fname ++ "' already exists"
      Nothing -> return $ VFields $ Map.insert fname VF{vfieldName = fname,
                                                        vfieldValue = val} fs

-- |Update an existing field of a record value. Fails if the field does not
-- already exist.
updateFieldValue :: FieldName -> e -> VFields e -> Either String (VFields e)
updateFieldValue fname val (VFields fs) =
    case Map.lookup fname fs of
      Just vf -> return $ VFields $ Map.insert fname vf {vfieldValue = val } fs
      Nothing -> Left $ "field '" ++ fname ++ "' is not defined"

-- |Set the field of a record value. If the field already exists it is updated,
-- otherwise it is added.
setFieldValue :: FieldName -> e -> VFields e -> VFields e
setFieldValue fname val (VFields fs) =
    case Map.lookup fname fs of
      Just vf -> VFields $ Map.insert fname vf {vfieldValue = val } fs
      Nothing -> VFields $ Map.insert fname VF{vfieldName = fname,
                                               vfieldValue = val} fs