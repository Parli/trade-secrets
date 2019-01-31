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
import Parli.Jux.Unwrap

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
juxEntityData :: JuxEntityType e => JuxUnwrapEntity' e a -> JuxEntityData e -> Maybe a
juxEntityData = juxDataChecking getJuxDataEntityType
juxAttributeData :: JuxEntityType e => JuxUnwrapAttribute' e a -> JuxAttributeData e -> Maybe a
juxAttributeData = juxDataChecking getJuxDataAttributeType
juxQueryData :: JuxQueryType q => JuxUnwrapQuery' q a -> JuxQueryRequest q -> Maybe a
juxQueryData = juxDataChecking getJuxRequestQuery
juxResponseData :: JuxQueryType q => JuxUnwrapResponse' q a -> JuxQueryResponse q -> Maybe a
juxResponseData = juxDataChecking getJuxResponseQuery

juxMapDataPairs :: JuxLabel a => JuxUnwrap a b c -> JuxMap a b Identity -> [(JuxKey a, c)]
juxMapDataPairs un = mapMaybe (sequence . go) . HM.toList . juxMapRestrictTypes typeSet
  where
  go (k, d) = (k,) . juxDataKeyed un k $ runIdentity d
  typeSet = HS.fromList $ juxUnwrapTypes un
juxMapData :: JuxLabel a => JuxUnwrap a b c -> JuxMap a b Identity -> [c]
juxMapData un = fmap snd . juxMapDataPairs un
juxMapDataPairsId :: JuxLabel a => JuxUnwrap a b c -> JuxMap a b Identity -> [(JuxId, c)]
juxMapDataPairsId un = fmap (first juxId) . juxMapDataPairs un
juxMapDataLookup :: JuxLabel a => JuxUnwrap a b c -> JuxKey a -> JuxMap a b Identity -> Maybe c
juxMapDataLookup un k j = juxDataKeyed un k =<< runIdentity <$> HM.lookup k j
juxMapDataLookupId :: JuxLabel a => JuxUnwrap a b c -> JuxId -> JuxMap a b Identity -> Maybe c
juxMapDataLookupId un k j = uncurry (juxDataKeyed un) =<< d
  where
  d = listToMaybe $ mapMaybe (sequence . go) altKeys
  go = id &&& fmap runIdentity . flip HM.lookup j
  altKeys = flip JuxKey k <$> juxUnwrapTypes un

juxStoreEntitiesData :: JuxEntityType e => JuxUnwrapEntity' e a -> JuxStore' e q -> [a]
juxStoreEntitiesData u = juxMapData u . juxEntities
juxStoreEntitiesDataPairs :: JuxEntityType e => JuxUnwrapEntity' e a -> JuxStore' e q -> [(JuxKey e, a)]
juxStoreEntitiesDataPairs u = juxMapDataPairs u . juxEntities
juxStoreEntitiesDataPairsId :: JuxEntityType e => JuxUnwrapEntity' e a -> JuxStore' e q -> [(JuxId, a)]
juxStoreEntitiesDataPairsId u = juxMapDataPairsId u . juxEntities
juxStoreEntitiesDataLookup :: JuxEntityType e => JuxUnwrapEntity' e a -> JuxKey e -> JuxStore' e q -> Maybe a
juxStoreEntitiesDataLookup u k = juxMapDataLookup u k . juxEntities
juxStoreEntitiesDataLookupId :: JuxEntityType e => JuxUnwrapEntity' e a -> JuxId -> JuxStore' e q -> Maybe a
juxStoreEntitiesDataLookupId u k = juxMapDataLookupId u k . juxEntities

juxStoreAttributesData :: JuxEntityType e => JuxUnwrapAttribute' e a -> JuxStore' e q -> [a]
juxStoreAttributesData u = juxMapData u . juxAttributes
juxStoreAttributesDataPairs :: JuxEntityType e => JuxUnwrapAttribute' e a -> JuxStore' e q -> [(JuxKey (JuxAttributeType e), a)]
juxStoreAttributesDataPairs u = juxMapDataPairs u . juxAttributes
juxStoreAttributesDataPairsId :: JuxEntityType e => JuxUnwrapAttribute' e a -> JuxStore' e q -> [(JuxId, a)]
juxStoreAttributesDataPairsId u = juxMapDataPairsId u . juxAttributes
juxStoreAttributesDataLookup :: JuxEntityType e => JuxUnwrapAttribute' e a -> JuxKey (JuxAttributeType e) -> JuxStore' e q -> Maybe a
juxStoreAttributesDataLookup u k = juxMapDataLookup u k . juxAttributes
juxStoreAttributesDataLookupId :: JuxEntityType e => JuxUnwrapAttribute' e a -> JuxId -> JuxStore' e q -> Maybe a
juxStoreAttributesDataLookupId u k = juxMapDataLookupId u k . juxAttributes

juxStoreQueriesData :: JuxQueryType q => JuxUnwrapQuery' q a -> JuxStore' e q -> [a]
juxStoreQueriesData u = juxMapData u . juxQueries
juxStoreQueriesDataPairs :: JuxQueryType q => JuxUnwrapQuery' q a -> JuxStore' e q -> [(JuxKey q, a)]
juxStoreQueriesDataPairs u = juxMapDataPairs u . juxQueries
juxStoreQueriesDataPairsId :: JuxQueryType q => JuxUnwrapQuery' q a -> JuxStore' e q -> [(JuxId, a)]
juxStoreQueriesDataPairsId u = juxMapDataPairsId u . juxQueries
juxStoreQueriesDataLookup :: JuxQueryType q => JuxUnwrapQuery' q a -> JuxKey q -> JuxStore' e q -> Maybe a
juxStoreQueriesDataLookup u k = juxMapDataLookup u k . juxQueries
juxStoreQueriesDataLookupId :: JuxQueryType q => JuxUnwrapQuery' q a -> JuxId -> JuxStore' e q -> Maybe a
juxStoreQueriesDataLookupId u k = juxMapDataLookupId u k . juxQueries

juxStoreResponsesData :: JuxQueryType q => JuxUnwrapResponse' q a -> JuxStore' e q -> [a]
juxStoreResponsesData u = juxMapData u . juxResponses
juxStoreResponsesDataPairs :: JuxQueryType q => JuxUnwrapResponse' q a -> JuxStore' e q -> [(JuxKey q, a)]
juxStoreResponsesDataPairs u = juxMapDataPairs u . juxResponses
juxStoreResponsesDataPairsId :: JuxQueryType q => JuxUnwrapResponse' q a -> JuxStore' e q -> [(JuxId, a)]
juxStoreResponsesDataPairsId u = juxMapDataPairsId u . juxResponses
juxStoreResponsesDataLookup :: JuxQueryType q => JuxUnwrapResponse' q a -> JuxKey q -> JuxStore' e q -> Maybe a
juxStoreResponsesDataLookup u k = juxMapDataLookup u k . juxResponses
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
