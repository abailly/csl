--------------------------------------------------------------------------------
-- |
-- Module      :  Poets.EntityStore
-- Copyright   :  3gERP, 2012
-- License     :  All Rights Reserved
-- 
-- Maintainer  :  Tom Hvitved
-- Stability   :  unknown
-- Portability :  unknown
--
-- The entity store. The actual in-memory representation is maintained by the
-- 'EventLog', which also takes care of persisting the correct events.
-- 
--------------------------------------------------------------------------------

module Poets.EntityStore
    (
     EntityStore,
     EntityStoreError(..),
     newEntityStore,
     createEntity,
     updateEntity,
     deleteEntity,
     latestEntityBefore,
     entitiesByType,
     getTypingEnv
    ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Poets.Data
import Control.Monad.Error

-- |Entity store.
data EntityStore = EntityStore { entityMap :: Map EntId Entity }

-- |An entity consists of a declared type, a history, and an indication whether
-- the entity is marked as deleted.
data Entity = Entity {
      entityType :: RecordName,
      entityHistory :: [(VRecord Value,DateTime)],
      entityDeleted :: Bool
    }

-- |Errors that may occur during run-time.
data EntityStoreError =
    -- |Entity does not exist.
    EntityNotFound EntId
    -- |Entity is deleted.
  | EntityDeleted EntId
    -- |Type error.
  | TypeError String

instance Show EntityStoreError where
    show (EntityNotFound eId) =
        "Entity " ++ show eId ++ ": Not found"
    show (EntityDeleted eId) =
        "Entity " ++ show eId ++ ": Deleted"
    show (TypeError e) =
        "Type error: " ++ e

instance Error EntityStoreError where
    strMsg = error

-- |Create a new, empty entity store.
newEntityStore :: EntityStore
newEntityStore = EntityStore Map.empty

-- |Create a new entity.
createEntity :: EntityStore -- ^Old entity store.
             -> POETSRecordEnv
             -> VRecord Value -- ^Entity data.
             -> RecordName -- ^Declared entity type.
             -> DateTime -- ^Time of creation.
             -> Either EntityStoreError (EntId, EntityStore)
createEntity store recordEnv v rName dt = do
    let emap = entityMap store
    let newId = if Map.null emap then 0 else fst (Map.findMax emap) + 1
    typeCheckEntityData recordEnv store rName v
    let ent = Entity rName [(v, dt)] False
    return (newId, EntityStore $ Map.insert newId ent emap)

-- |Update an existing entity.
updateEntity :: EntityStore -- ^Old entity store.
             -> POETSRecordEnv
             -> EntId -- ^Entity ID.
             -> VRecord Value -- ^Entity data.
             -> DateTime -- ^Time of update.
             -> Either EntityStoreError EntityStore
updateEntity store recordEnv id v dt =
    let emap = entityMap store in
    case Map.lookup id emap of
      Nothing ->
          throwError $ EntityNotFound id
      Just entity -> do
          typeCheckEntityData recordEnv store (entityType entity) v
          let ent e = Just (Entity (entityType e)
                                   ((v,dt) : entityHistory e)
                                   False)
          return $ EntityStore $ Map.update ent id emap

-- |Delete an existing entity.
deleteEntity :: EntityStore -- ^Old entity store.
             -> EntId -- ^Entity ID.
             -> Either EntityStoreError (RecordName, EntityStore)
deleteEntity store id =
    let emap = entityMap store in
    case Map.lookup id emap of
      Nothing ->
          throwError $ EntityNotFound id
      Just entity
        | entityDeleted entity ->
            throwError $ EntityDeleted id
        | otherwise ->
            return (entityType entity,
                    EntityStore $
                    Map.update (\e -> Just e{entityDeleted = True}) id emap)

typeCheckEntityData :: POETSRecordEnv
                    -> EntityStore
                    -> RecordName
                    -> VRecord Value
                    -> Either EntityStoreError ()
typeCheckEntityData recordEnv store rName v =
    either (throwError . TypeError)
           return
           (typeCheckRecord recordEnv (getTypingEnv False store) rName (iVRecord v))

-- |Extract the entity typing environment. The Boolean indicates whether we want
-- the accumulated typing environment or not.
getTypingEnv :: Bool -> EntityStore -> EntityTypingEnv
getTypingEnv acc = Map.map entityType .
                   Map.filter (\e -> acc || not (entityDeleted e)) . entityMap

-- |Lookup the latest version of an entity before the specified time.
latestEntityBefore :: EntityStore
                   -> EntId
                   -> Maybe DateTime
                   -> Either EntityStoreError (VRecord Value)
latestEntityBefore (EntityStore store) eId mDt =
  case Map.lookup eId store of
    Nothing ->
        throwError $ EntityNotFound eId
    Just entity
         | entityDeleted entity ->
             throwError $ EntityDeleted eId
         | otherwise ->
             case [r | (r,dt) <- entityHistory entity,
                       maybe True (<= dt) mDt] of
               r : _ -> return r
               [] -> throwError $ EntityNotFound eId

-- |Retrieve the latest version of (non-deleted) entities by type.
entitiesByType :: EntityStore -> POETSRecordEnv -> Type -> [(EntId, Value)]
entitiesByType store recordEnv tp =
    [(id,iVRecord v) | (id,e) <- Map.toList $ entityMap store,
                       not $ entityDeleted e,
                       let (v,_) = head $ entityHistory e,
                       checkSubType recordEnv (iTRecord $ vrecordName v) tp == Right ()]