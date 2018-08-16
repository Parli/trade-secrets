{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Parli.Jux.Types
( JuxValue, JuxLabel, JuxEntityType(..), JuxQueryType(..), JuxStoreType
, JuxRawId, JuxId(..), JuxIdMap
, JuxAttributes', JuxEntities', JuxQueries', JuxResponses', JuxTypes'
, JuxStore'(..)
) where

import           Data.Aeson
import           RIO
import qualified RIO.HashMap as HM

type JuxValue a = (Eq a, Show a, ToJSON a, FromJSON a)
type JuxLabel a = (JuxValue a, Read a, Hashable a, ToJSONKey a, FromJSONKey a)

class
  ( JuxLabel a, JuxLabel (JuxAttributeType a)
  , JuxValue (JuxEntityData a), JuxValue (JuxAttributeData a)
  ) => JuxEntityType a where
  type JuxAttributeType a = x | x -> a
  type JuxEntityData    a = y | y -> a
  type JuxAttributeData a = z | z -> a
  getJuxAttributeEntityType :: JuxAttributeType a -> a
  getJuxDataEntityType      :: JuxEntityData a -> a
  getJuxDataAttributeType   :: JuxAttributeData a -> JuxAttributeType a

class
  ( JuxLabel a
  , JuxValue (JuxQueryRequest a), JuxValue (JuxQueryResponse a)
  ) => JuxQueryType a where
  type JuxQueryRequest  a = x | x -> a
  type JuxQueryResponse a = y | y -> a
  getJuxRequestQuery  :: JuxQueryRequest a -> a
  getJuxResponseQuery :: JuxQueryResponse a -> a

type JuxStoreType e q = (JuxEntityType e, JuxQueryType q)

type JuxRawId = Text
data JuxId a = JuxId
  { juxType  :: a
  , juxRawId :: JuxRawId
  } deriving (Eq, Ord, Show, Data, Typeable, Generic, Hashable)

type JuxIdMap a b f = HashMap (JuxId a) (f b)

-- Types with a tick are intended to be aliased to a concrete type, e.g.:
-- type JuxStore = JuxStore' MyEntity MyQuery
type JuxAttributes' e f = JuxIdMap (JuxAttributeType e) (JuxAttributeData e) f
type JuxEntities' e f = JuxIdMap e (JuxEntityData e) f
type JuxQueries' q f = JuxIdMap q (JuxQueryRequest q) f
type JuxResponses' q f = JuxIdMap q (JuxQueryResponse q) f
type JuxTypes' e = HashMap JuxRawId e

data JuxStore' e q = JuxStore
  { juxAttributes :: JuxAttributes' e Identity
  , juxEntities   :: JuxEntities' e Identity
  , juxQueries    :: JuxQueries' q Identity
  , juxResponses  :: JuxResponses' q Identity
  , juxTypes      :: JuxTypes' e
  } deriving (Generic, Typeable)
deriving instance JuxStoreType e q => Eq (JuxStore' e q)
deriving instance JuxStoreType e q => Show (JuxStore' e q)
instance JuxStoreType e q => ToJSON (JuxStore' e q) where
  toJSON = toJSON . juxStoreToWire
instance JuxStoreType e q => FromJSON (JuxStore' e q) where
  parseJSON = fmap juxWireToStore . parseJSON
instance JuxStoreType e q => Semigroup (JuxStore' e q) where
  (JuxStore a1 e1 q1 r1 t1) <> (JuxStore a2 e2 q2 r2 t2)
    = JuxStore -- favor keys in right-hand argument
      (HM.union a2 a1) (HM.union e2 e1)
      (HM.union q2 q1) (HM.union r2 r1)
      (HM.union t2 t1)
instance JuxStoreType e q => Monoid (JuxStore' e q) where
  mempty = JuxStore mempty mempty mempty mempty mempty
  mappend = (<>)

-- Intermediate structure for serialization
type JuxWireMap l v = HashMap l (HashMap JuxRawId v)
data JuxWire e q = JuxWire
  { attributes :: HashMap e (JuxWireMap (JuxAttributeType e) (JuxAttributeData e))
  , entities   :: JuxWireMap e (JuxEntityData e)
  , queries    :: JuxWireMap q (JuxQueryRequest q)
  , responses  :: JuxWireMap q (JuxQueryResponse q)
  , types      :: JuxTypes' e
  } deriving (Generic, Typeable)
-- deriving instance JuxStoreType e q => Show (JuxWire e q)
deriving instance JuxStoreType e q => ToJSON (JuxWire e q)
deriving instance JuxStoreType e q => FromJSON (JuxWire e q)

juxWireToStore :: JuxStoreType e q => JuxWire e q -> JuxStore' e q
juxWireToStore (JuxWire a e q r t) = JuxStore a' e' q' r' t
  where
    a' = toIdMap . fold . HM.elems $ a
    e' = toIdMap e
    q' = toIdMap q
    r' = toIdMap r
    toIdMap :: (JuxLabel l) => JuxWireMap l v -> JuxIdMap l v Identity
    toIdMap = HM.fromList . concatMap (uncurry toIdPair) . HM.toList
    toIdPair k = fmap (JuxId k *** Identity) . HM.toList

juxStoreToWire :: JuxStoreType e q => JuxStore' e q -> JuxWire e q
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
