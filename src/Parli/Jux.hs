module Parli.Jux
( JuxValue, JuxLabel, JuxEntityType(..), JuxQueryType(..), JuxStoreType
, JuxRawId, JuxId(..)
, JuxAttributes, JuxEntities, JuxQueries, JuxResponses, JuxTypes
, JuxStore(..), JuxWire()
, module Parli.Jux
) where

import           RIO
import qualified RIO.HashMap as HM
import qualified RIO.Text as T

import Parli.Jux.Types
import Text.Casing
import Text.Read

-- convenient constructors
juxStorable :: (Hashable a) => JuxId a -> b -> JuxIdMap a b Identity
juxStorable k = HM.singleton k . Identity

storeAttributes :: JuxStoreType e q => JuxAttributes e Identity -> JuxStore e q
storeAttributes xs = mempty { juxAttributes = xs }
storeEntities :: JuxStoreType e q => JuxEntities e Identity -> JuxStore e q
storeEntities xs = mempty { juxEntities = xs }
storeQueries :: JuxStoreType e q => JuxQueries q Identity -> JuxStore e q
storeQueries xs = mempty { juxQueries = xs }
storeResponses :: JuxStoreType e q => JuxResponses q Identity -> JuxStore e q
storeResponses xs = mempty { juxResponses = xs }

juxStorableType :: (Hashable e) => JuxId e -> HashMap e Text
juxStorableType = HM.singleton . juxType <*> juxRawId

storeTypes :: JuxStoreType e q => JuxTypes e -> JuxStore e q
storeTypes xs = mempty { juxTypes = xs }

-- constrained morphisms
juxMorph :: (JuxId a -> f b -> g c) -> JuxIdMap a b f -> JuxIdMap a c g
juxMorph = HM.mapWithKey

juxTraverse :: (Monad m)
  => (JuxId a -> f b -> m (g c)) -> JuxIdMap a b f -> m (JuxIdMap a c g)
juxTraverse action = sequence . HM.mapWithKey action

juxResolve :: JuxIdMap a b Maybe -> JuxIdMap a b Identity
juxResolve = HM.mapMaybe (fmap Identity)

-- (de)serialization
showJuxLabel :: (JuxLabel a) => a -> Text
showJuxLabel = fromString . toQuietSnake . fromHumps . show

readJuxLabel :: (JuxLabel a) => (Text -> e) -> Text -> Either e a
readJuxLabel toError t = case reads s of
  [(a,[])] -> Right a
  _        -> Left (toError t)
  where s = toPascal . fromSnake . T.unpack $ t

juxStoreToWire :: JuxStoreType e q => JuxStore e q -> JuxWire e q
juxStoreToWire (JuxStore a e q r t) = JuxWire a' e' q' r' t
  where
    a' = HM.fromListWith (<>) . fmap toWireAttributePair . toWirePairs $ a
    e' = toWireMap e
    q' = toWireMap q
    r' = toWireMap r
    toWireMap :: (JuxLabel l) => JuxIdMap l v Identity -> JuxWireMap l v
    toWireMap = HM.fromList . toWirePairs
    toWirePairs = fmap (uncurry embedMap . toWirePair) . HM.toList
    toWirePair = (juxType &&& juxRawId) *** runIdentity
    embedMap (l, k) = (l,) . HM.singleton k
    toWireAttributePair = getJuxAttributeEntityType . fst &&& uncurry HM.singleton

juxWireToStore :: JuxStoreType e q => JuxWire e q -> JuxStore e q
juxWireToStore (JuxWire a e q r t) = JuxStore a' e' q' r' t
  where
    a' = toIdMap . fold . HM.elems $ a
    e' = toIdMap e
    q' = toIdMap q
    r' = toIdMap r
    toIdMap :: (JuxLabel l) => JuxWireMap l v -> JuxIdMap l v Identity
    toIdMap = HM.fromList . concatMap (uncurry toIdPair) . HM.toList
    toIdPair k = fmap (JuxId k *** Identity) . HM.toList
