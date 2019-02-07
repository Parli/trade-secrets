{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module Parli.Jux.Core.Types
( JuxValue, JuxLabel, JuxEntityType(..), JuxQueryType(..), JuxStoreType
, JuxId, JuxKey(..), JuxMap
, JuxAttributes', JuxEntities', JuxQueries', JuxResponses', JuxTypes'
, JuxStore'(..)
, JuxWireMap(..), JuxLabelValue(..)
) where

import           RIO
import qualified RIO.HashMap as HM

import Codec.Serialise
import Data.Aeson.Types
import Data.Tuple
import Parli.Jux.Internal

import Data.Aeson.Encoding.Internal
import Data.Binary.Builder
import Data.Text.Encoding -- unneeded for decodeUtf8'

type JuxValue a = (Eq a, Show a, ToJSON a, FromJSON a, NFData a)
type JuxLabel a = (JuxValue a, Ord a, Hashable a, Read a, ToJSONKey a, FromJSONKey a)

class
  ( JuxLabel a, JuxLabel (JuxAttributeType a)
  , JuxValue (JuxEntityData a), JuxValue (JuxAttributeData a)
  ) => JuxEntityType a where
  type JuxAttributeType a = x | x -> a
  type JuxEntityData    a = y | y -> a
  type JuxAttributeData a = z | z -> a
  getJuxDataEntityType      :: JuxEntityData a -> a
  getJuxDataAttributeType   :: JuxAttributeData a -> JuxAttributeType a
  getJuxAttributeEntityType :: JuxAttributeType a -> a

class
  ( JuxLabel a
  , JuxValue (JuxQueryRequest a), JuxValue (JuxQueryResponse a)
  ) => JuxQueryType a where
  type JuxQueryRequest  a = x | x -> a
  type JuxQueryResponse a = y | y -> a
  getJuxRequestQuery  :: JuxQueryRequest a -> a
  getJuxResponseQuery :: JuxQueryResponse a -> a

type JuxStoreType e q = (JuxEntityType e, JuxQueryType q)
type JuxWireType e q =
  ( JuxStoreType e q
  , JuxLabelValue e (JuxEntityData e)
  , JuxLabelValue (JuxAttributeType e) (JuxAttributeData e)
  , JuxLabelValue q (JuxQueryRequest q)
  , JuxLabelValue q (JuxQueryResponse q)
  )

newtype JuxId = JuxId { juxIdBytes :: ByteString }
  deriving newtype (Eq, Ord, Show, Read, Typeable, Hashable, NFData, Serialise)
  deriving anyclass (Data, Generic)
instance Display JuxId where
  display = displayBytesUtf8 . juxIdBytes
instance FromJSON JuxId where
  parseJSON = fmap (JuxId . encodeUtf8) . parseJSON
instance ToJSON JuxId where
  toJSON = toJSON . decodeUtf8 . juxIdBytes -- throws on failed decode!
  -- toJSON = toJSON . either (const "") id . decodeUtf8'
  toEncoding = Encoding . fromByteString . juxIdBytes
instance FromJSONKey JuxId
instance ToJSONKey JuxId

data JuxKey a = JuxKey
  { juxType :: a
  , juxId   :: JuxId
  } deriving (Eq, Ord, Show, Data, Typeable, Generic, Hashable, NFData)
deriving instance Serialise a => Serialise (JuxKey a)

type JuxMap a b f = HashMap (JuxKey a) (f b)

-- Types with a tick are intended to be aliased to a concrete type, e.g.:
-- type JuxStore = JuxStore' MyEntity MyQuery
type JuxAttributes' e f = JuxMap (JuxAttributeType e) (JuxAttributeData e) f
type JuxEntities' e f = JuxMap e (JuxEntityData e) f
type JuxQueries' q f = JuxMap q (JuxQueryRequest q) f
type JuxResponses' q f = JuxMap q (JuxQueryResponse q) f
type JuxTypes' e = HashMap JuxId e

data JuxStore' e q = JuxStore
  { juxAttributes :: JuxAttributes' e Identity
  , juxEntities   :: JuxEntities' e Identity
  , juxQueries    :: JuxQueries' q Identity
  , juxResponses  :: JuxResponses' q Identity
  , juxTypes      :: JuxTypes' e
  } deriving (Generic, Typeable)
deriving instance JuxStoreType e q => Eq (JuxStore' e q)
deriving instance JuxStoreType e q => Show (JuxStore' e q)
deriving instance JuxStoreType e q => NFData (JuxStore' e q)
deriving instance
  ( JuxStoreType e q, Serialise e, Serialise (JuxEntityData e)
  , Serialise (JuxAttributeType e), Serialise (JuxAttributeData e)
  , Serialise q, Serialise (JuxQueryRequest q), Serialise (JuxQueryResponse q)
  ) => Serialise (JuxStore' e q)
instance JuxWireType e q => ToJSON (JuxStore' e q) where
  toJSON = toJSON . juxStoreToWire
instance JuxWireType e q => FromJSON (JuxStore' e q) where
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
newtype JuxWireMap l v
  = JuxWireMap { getJuxWireMap :: (HashMap l (HashMap JuxId v)) }
  deriving (Eq, Show, Read, Generic)
instance (JuxLabel l) => Semigroup (JuxWireMap l v) where
  (JuxWireMap x) <> (JuxWireMap y) = JuxWireMap $ x <> y
instance (JuxLabel l) => Monoid (JuxWireMap l v) where
  mempty = JuxWireMap mempty
  mappend = (<>)
instance (JuxLabelValue l v) => ToJSON (JuxWireMap l v) where
  toJSON = toJSON . getJuxWireMap
instance (JuxLabelValue l v) => FromJSON (JuxWireMap l v) where
  parseJSON = withObject "JuxWireMap"
    $ fmap (JuxWireMap . HM.fromList) . traverse (sequence . uncurry go)
    . mapMaybe (sequenceFst . first readJuxLabelMaybe) . HM.toList
    where
      go l = (l,) . withObject "JuxRawIdMap" (readInnerMap l)
      readInnerMap l = fmap HM.fromList . traverse (readPair l) . HM.toList
      readPair l (k,v) = sequence (JuxId $ encodeUtf8 k, juxLabelValueParseJSON l v)
      readJuxLabelMaybe = either (const Nothing) Just . readJuxLabel undefined
      sequenceFst = fmap swap . sequence . swap

class (JuxLabel l, JuxValue v) => JuxLabelValue l v where
  juxLabelValueParseJSON :: l -> Value -> Parser v

data JuxWire e q = JuxWire
  { attributes :: Maybe (HashMap e (JuxWireMap (JuxAttributeType e) (JuxAttributeData e)))
  , entities   :: Maybe (JuxWireMap e (JuxEntityData e))
  , queries    :: Maybe (JuxWireMap q (JuxQueryRequest q))
  , responses  :: Maybe (JuxWireMap q (JuxQueryResponse q))
  , types      :: Maybe (JuxTypes' e)
  } deriving (Generic, Typeable)
-- deriving instance JuxStoreType e q => Show (JuxWire e q)
deriving instance JuxWireType e q => ToJSON (JuxWire e q)
deriving instance JuxWireType e q => FromJSON (JuxWire e q)

juxWireToStore :: JuxStoreType e q => JuxWire e q -> JuxStore' e q
juxWireToStore (JuxWire a e q r t) = JuxStore a' e' q' r' t'
  where
    a' = maybe mempty (toIdMap . fold . HM.elems) a
    e' = maybe mempty toIdMap e
    q' = maybe mempty toIdMap q
    r' = maybe mempty toIdMap r
    t' = maybe mempty id t
    toIdMap :: (JuxLabel l) => JuxWireMap l v -> JuxMap l v Identity
    toIdMap = HM.fromList . concatMap (uncurry toIdPair) . HM.toList . getJuxWireMap
    toIdPair k = fmap (JuxKey k *** Identity) . HM.toList

juxStoreToWire :: JuxStoreType e q => JuxStore' e q -> JuxWire e q
juxStoreToWire (JuxStore a e q r t) = JuxWire a' e' q' r' t'
  where
    a' = Just . HM.fromListWith (<>) . fmap toWireAttributePair . toWirePairs $ a
    e' = Just . toWireMap $ e
    q' = Just . toWireMap $ q
    r' = Just . toWireMap $ r
    t' = Just t
    toWireMap :: (JuxLabel l) => JuxMap l v Identity -> JuxWireMap l v
    toWireMap = JuxWireMap . HM.fromListWith (<>) . toWirePairs
    toWirePairs = fmap (uncurry embedMap . toWirePair) . HM.toList
    toWirePair = (juxType &&& juxId) *** runIdentity
    embedMap (l, k) = (l,) . HM.singleton k
    toWireAttributePair = getJuxAttributeEntityType . fst &&& JuxWireMap . uncurry HM.singleton
