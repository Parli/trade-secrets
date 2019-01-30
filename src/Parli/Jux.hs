module Parli.Jux
( module Parli.Jux
, module Parli.Jux.Core
, module Parli.Jux.Internal
) where

import           RIO
import qualified RIO.HashMap as HM
import qualified RIO.HashSet as HS

import Parli.Jux.Core
import Parli.Jux.Internal

-- convenient constructors
juxStorable :: (Hashable a) => JuxKey a -> b -> JuxMap a b Identity
juxStorable k = HM.singleton k . Identity

storeAttributes :: JuxStoreType e q => JuxAttributes' e Identity -> JuxStore' e q
storeAttributes xs = mempty { juxAttributes = xs }
storeEntities :: JuxStoreType e q => JuxEntities' e Identity -> JuxStore' e q
storeEntities xs = mempty { juxEntities = xs }
storeQueries :: JuxStoreType e q => JuxQueries' q Identity -> JuxStore' e q
storeQueries xs = mempty { juxQueries = xs }
storeResponses :: JuxStoreType e q => JuxResponses' q Identity -> JuxStore' e q
storeResponses xs = mempty { juxResponses = xs }

juxStorableType :: (Hashable e) => JuxKey e -> HashMap Text e
juxStorableType = HM.singleton . juxId <*> juxType

storeTypes :: JuxStoreType e q => JuxTypes' e -> JuxStore' e q
storeTypes xs = mempty { juxTypes = xs }

-- constrained morphisms
juxMorph :: (JuxKey a -> f b -> g c) -> JuxMap a b f -> JuxMap a c g
juxMorph = HM.mapWithKey

juxTraverse :: (Monad m)
  => (JuxKey a -> f b -> m (g c)) -> JuxMap a b f -> m (JuxMap a c g)
juxTraverse action = sequence . HM.mapWithKey action

juxResolve :: JuxMap a b Maybe -> JuxMap a b Identity
juxResolve = HM.mapMaybe (fmap Identity)

toAttributeKey :: (a ~ JuxAttributeType e) => a -> JuxKey e -> JuxKey a
toAttributeKey a k = k { juxType = a }

toEntityKey :: (JuxEntityType e, a ~ JuxAttributeType e) => JuxKey a -> JuxKey e
toEntityKey k@JuxKey{ juxType = a } = k{ juxType = getJuxAttributeEntityType a }

-- filtration
juxKeyHasType :: Eq a => a -> JuxKey a -> Bool
juxKeyHasType t k = juxType k == t

juxMapFilterKeys :: (k -> Bool) -> HashMap k v -> HashMap k v
juxMapFilterKeys f = HM.filterWithKey $ flip (const f)

juxMapRestrictIds :: HashSet JuxId -> JuxMap a b f -> JuxMap a b f
juxMapRestrictIds ks = juxMapFilterKeys $ flip HS.member ks . juxId

juxMapRestrictTypes :: (Eq a, Hashable a) => HashSet a -> JuxMap a b f -> JuxMap a b f
juxMapRestrictTypes ts = juxMapFilterKeys $ flip HS.member ts . juxType

juxTypesRestrictIds :: HashSet JuxId -> JuxTypes' e -> JuxTypes' e
juxTypesRestrictIds ks = juxMapFilterKeys $ flip HS.member ks

juxTypesRestrictTypes :: (Eq e, Hashable e) => HashSet e -> JuxTypes' e -> JuxTypes' e
juxTypesRestrictTypes ts = HM.filter $ flip HS.member ts

-- extraction
juxDataUnsafe :: JuxUnwrap a b c -> b -> c
juxDataUnsafe (JuxUnwrap _ un) = un
juxDataChecking :: (b -> a) -> JuxUnwrap a b c -> b -> Maybe c
juxDataChecking is (JuxUnwrap t un) d
  = if is d == t then Just (un d) else Nothing

juxEntityData :: JuxEntityType e => JuxUnwrapEntity' e a -> JuxEntityData e -> Maybe a
juxEntityData = juxDataChecking getJuxDataEntityType
juxAttributeData :: JuxEntityType e => JuxUnwrapAttribute' e a -> JuxAttributeData e -> Maybe a
juxAttributeData = juxDataChecking getJuxDataAttributeType
juxQueryData :: JuxQueryType q => JuxUnwrapQuery' q a -> JuxQueryRequest q -> Maybe a
juxQueryData = juxDataChecking getJuxRequestQuery
juxResponseData :: JuxQueryType q => JuxUnwrapResponse' q a -> JuxQueryResponse q -> Maybe a
juxResponseData = juxDataChecking getJuxResponseQuery

juxMapData :: JuxUnwrap a b c -> JuxMap a b Identity -> [c]
juxMapData (JuxUnwrap t un)
  = fmap (un . runIdentity) . HM.elems . juxMapFilterKeys (juxKeyHasType t)
juxMapDataPairs :: JuxUnwrap a b c -> JuxMap a b Identity -> [(JuxId, c)]
juxMapDataPairs (JuxUnwrap t un)
  = fmap (juxId *** un . runIdentity) . HM.toList . juxMapFilterKeys (juxKeyHasType t)
juxMapDataLookupKey :: JuxUnwrap a b c -> JuxKey a -> JuxMap a b Identity -> Maybe c
juxMapDataLookupKey (JuxUnwrap _ un) k = fmap (un . runIdentity) . HM.lookup k
juxMapDataLookupId :: JuxUnwrap a b c -> JuxId -> JuxMap a b Identity -> Maybe c
juxMapDataLookupId u@(JuxUnwrap t _) k = juxMapDataLookupKey u (JuxKey t k)

juxStoreEntitiesData :: JuxEntityType e => JuxUnwrapEntity' e a -> JuxStore' e q -> [a]
juxStoreEntitiesData u = juxMapData u . juxEntities
juxStoreEntitiesDataPairs :: JuxEntityType e => JuxUnwrapEntity' e a -> JuxStore' e q -> [(JuxId, a)]
juxStoreEntitiesDataPairs u = juxMapDataPairs u . juxEntities
juxStoreEntitiesDataLookupKey :: JuxEntityType e => JuxUnwrapEntity' e a -> JuxKey e -> JuxStore' e q -> Maybe a
juxStoreEntitiesDataLookupKey u k = juxMapDataLookupKey u k . juxEntities
juxStoreEntitiesDataLookupId :: JuxEntityType e => JuxUnwrapEntity' e a -> JuxId -> JuxStore' e q -> Maybe a
juxStoreEntitiesDataLookupId u k = juxMapDataLookupId u k . juxEntities

juxStoreAttributesData :: JuxEntityType e => JuxUnwrapAttribute' e a -> JuxStore' e q -> [a]
juxStoreAttributesData u = juxMapData u . juxAttributes
juxStoreAttributesDataPairs :: JuxEntityType e => JuxUnwrapAttribute' e a -> JuxStore' e q -> [(JuxId, a)]
juxStoreAttributesDataPairs u = juxMapDataPairs u . juxAttributes
juxStoreAttributesDataLookupKey :: JuxEntityType e => JuxUnwrapAttribute' e a -> JuxKey (JuxAttributeType e) -> JuxStore' e q -> Maybe a
juxStoreAttributesDataLookupKey u k = juxMapDataLookupKey u k . juxAttributes
juxStoreAttributesDataLookupId :: JuxEntityType e => JuxUnwrapAttribute' e a -> JuxId -> JuxStore' e q -> Maybe a
juxStoreAttributesDataLookupId u k = juxMapDataLookupId u k . juxAttributes

juxStoreQueriesData :: JuxQueryType q => JuxUnwrapQuery' q a -> JuxStore' e q -> [a]
juxStoreQueriesData u = juxMapData u . juxQueries
juxStoreQueriesDataPairs :: JuxQueryType q => JuxUnwrapQuery' q a -> JuxStore' e q -> [(JuxId, a)]
juxStoreQueriesDataPairs u = juxMapDataPairs u . juxQueries
juxStoreQueriesDataLookupKey :: JuxQueryType q => JuxUnwrapQuery' q a -> JuxKey q -> JuxStore' e q -> Maybe a
juxStoreQueriesDataLookupKey u k = juxMapDataLookupKey u k . juxQueries
juxStoreQueriesDataLookupId :: JuxQueryType q => JuxUnwrapQuery' q a -> JuxId -> JuxStore' e q -> Maybe a
juxStoreQueriesDataLookupId u k = juxMapDataLookupId u k . juxQueries

juxStoreResponsesData :: JuxQueryType q => JuxUnwrapResponse' q a -> JuxStore' e q -> [a]
juxStoreResponsesData u = juxMapData u . juxResponses
juxStoreResponsesDataPairs :: JuxQueryType q => JuxUnwrapResponse' q a -> JuxStore' e q -> [(JuxId, a)]
juxStoreResponsesDataPairs u = juxMapDataPairs u . juxResponses
juxStoreResponsesDataLookupKey :: JuxQueryType q => JuxUnwrapResponse' q a -> JuxKey q -> JuxStore' e q -> Maybe a
juxStoreResponsesDataLookupKey u k = juxMapDataLookupKey u k . juxResponses
juxStoreResponsesDataLookupId :: JuxQueryType q => JuxUnwrapResponse' q a -> JuxId -> JuxStore' e q -> Maybe a
juxStoreResponsesDataLookupId u k = juxMapDataLookupId u k . juxResponses

-- element operations
juxLookupType :: JuxStoreType e q => JuxId -> JuxStore' e q -> Maybe e
juxLookupType k = HM.lookup k . juxTypes

juxLookupKey :: JuxStoreType e q => JuxId -> JuxStore' e q -> Maybe (JuxKey e)
juxLookupKey k = fmap (flip JuxKey k) . juxLookupType k

juxLookupEntity :: JuxStoreType e q
  => JuxKey e -> JuxStore' e q -> Maybe (JuxEntityData e)
juxLookupEntity k = fmap runIdentity . HM.lookup k . juxEntities

juxLookupAttribute :: JuxStoreType e q
  => JuxKey (JuxAttributeType e) -> JuxStore' e q -> Maybe (JuxAttributeData e)
juxLookupAttribute k = fmap runIdentity . HM.lookup k . juxAttributes

juxLookupQuery :: JuxStoreType e q
  => JuxKey q -> JuxStore' e q -> Maybe (JuxQueryRequest q)
juxLookupQuery k = fmap runIdentity . HM.lookup k . juxQueries

juxLookupResponse :: JuxStoreType e q
  => JuxKey q -> JuxStore' e q -> Maybe (JuxQueryResponse q)
juxLookupResponse k = fmap runIdentity . HM.lookup k . juxResponses

-- juxInsertType :: JuxStoreType e q
--   => JuxKey e -> JuxStore' e q -> JuxStore' e q
-- juxInsertType (JuxKey v k) jux@JuxStore{juxTypes}
--   = jux{ juxTypes = HM.insert k v juxTypes }

-- juxInsertEntity :: JuxStoreType e q
--   => JuxKey e -> JuxEntityData e -> JuxStore' e q -> JuxStore' e q
-- juxInsertEntity k v jux@JuxStore{juxEntities}
--   = jux{ juxEntities = HM.insert k (Identity v) juxEntities }

-- juxInsertAttribute :: JuxStoreType e q
--   => JuxKey (JuxAttributeType e) -> JuxAttributeData e -> JuxStore' e q -> JuxStore' e q
-- juxInsertAttribute k v jux@JuxStore{juxAttributes}
--   = jux{ juxAttributes = HM.insert k (Identity v) juxAttributes }

-- juxInsertQuery :: JuxStoreType e q
--   => JuxKey q -> JuxQueryRequest q -> JuxStore' e q -> JuxStore' e q
-- juxInsertQuery k v jux@JuxStore{juxQueries}
--   = jux{ juxQueries = HM.insert k (Identity v) juxQueries }

-- juxInsertResponse :: JuxStoreType e q
--   => JuxKey q -> JuxQueryResponse q -> JuxStore' e q -> JuxStore' e q
-- juxInsertResponse k v jux@JuxStore{juxResponses}
--   = jux{ juxResponses = HM.insert k (Identity v) juxResponses }

-- juxDeleteType :: JuxStoreType e q => JuxId -> JuxStore' e q -> JuxStore' e q
-- juxDeleteType k jux@JuxStore{juxTypes} = jux{ juxTypes = HM.delete k juxTypes}

-- juxDeleteEntity :: JuxStoreType e q => JuxKey e -> JuxStore' e q -> JuxStore' e q
-- juxDeleteEntity k jux@JuxStore{juxEntities} = jux{ juxEntities = HM.delete k juxEntities}

-- juxDeleteAttribute :: JuxStoreType e q => JuxKey (JuxAttributeType e) -> JuxStore' e q -> JuxStore' e q
-- juxDeleteAttribute k jux@JuxStore{juxAttributes} = jux{ juxAttributes = HM.delete k juxAttributes}

-- juxDeleteQuery :: JuxStoreType e q => JuxKey q -> JuxStore' e q -> JuxStore' e q
-- juxDeleteQuery k jux@JuxStore{juxQueries} = jux{ juxQueries = HM.delete k juxQueries}

-- juxDeleteResponse :: JuxStoreType e q => JuxKey q -> JuxStore' e q -> JuxStore' e q
-- juxDeleteResponse k jux@JuxStore{juxResponses} = jux{ juxResponses = HM.delete k juxResponses}
