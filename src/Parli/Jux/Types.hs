{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module Parli.Jux.Types
( JuxValue, JuxLabel, JuxEntityType(..), JuxQueryType(..), JuxStoreType
, JuxRawId, JuxId(..), JuxIdMap
, JuxAttributes', JuxEntities', JuxQueries', JuxResponses', JuxTypes'
, JuxStore'(..)
, JuxWireMap(..), JuxLabelValue(..), wrapParseJSON
, juxLabelToWire, juxWireToLabel, showJuxLabel, readJuxLabel, juxReadError
) where

-- TODO: factor into .Types, .Types.Internal, .Wire

import           RIO
import qualified RIO.HashMap as HM

import           Data.Aeson
import qualified Data.Aeson.Types as Aeson

import qualified RIO.Text as T
import           Text.Casing
import           Text.Read

type JuxValue a = (Eq a, Show a, ToJSON a, FromJSON a)
type JuxLabel a = (JuxValue a, Read a, Hashable a, ToJSONKey a, FromJSONKey a)

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
  = JuxWireMap { getJuxWireMap :: (HashMap l (HashMap JuxRawId v)) }
  deriving (Eq, Show, Read, Generic, ToJSON)
instance (JuxLabel l) => Semigroup (JuxWireMap l v) where
  (JuxWireMap x) <> (JuxWireMap y) = JuxWireMap $ x <> y
instance (JuxLabel l) => Monoid (JuxWireMap l v) where
  mempty = JuxWireMap mempty
  mappend = (<>)
instance (JuxLabelValue l v) => FromJSON (JuxWireMap l v) where
  parseJSON = withObject "JuxWireMap" $ \o -> do
    let
      readJuxLabel' e = either error id . readJuxLabel (juxReadError e)
      raw :: [(Text, Value)]
      raw = HM.toList o
      juxKeys :: [(l, Value)]
      juxKeys = first (readJuxLabel' "JuxLabelValue") <$> raw
      juxPairs :: Aeson.Parser [(l, HashMap JuxRawId v)]
      juxPairs = sequence $ sequence . go <$> juxKeys
      go :: (l, Value) -> (l, Aeson.Parser (HashMap JuxRawId v))
      go (l, m) = (l, innerParse l m)
      innerParse :: l -> Value -> Aeson.Parser (HashMap JuxRawId v)
      innerParse l = withObject "JuxRawIdMap" $ \m -> do
        let
          innerRaw :: [(Text, Value)]
          innerRaw = HM.toList m
          innerKeys :: [(JuxRawId, Value)]
          innerKeys = first (readJuxLabel' "JuxRawIdMap") <$> innerRaw
          innerPairs :: Aeson.Parser [(JuxRawId, v)]
          innerPairs = sequence $ sequence . innerGo <$> innerKeys
          innerGo :: (JuxRawId, Value) -> (JuxRawId, Aeson.Parser v)
          innerGo (k, value) = (k, juxLabelValueParseJSON l value)
        HM.fromList <$> innerPairs
    JuxWireMap . HM.fromList <$> juxPairs

class (JuxLabel l, JuxValue v) => JuxLabelValue l v where
  juxLabelValueParseJSON :: l -> Value -> Aeson.Parser v

wrapParseJSON :: FromJSON a => (a -> v) -> Value -> Aeson.Parser v
wrapParseJSON w = fmap w . parseJSON

data JuxWire e q = JuxWire
  { attributes :: HashMap e (JuxWireMap (JuxAttributeType e) (JuxAttributeData e))
  , entities   :: JuxWireMap e (JuxEntityData e)
  , queries    :: JuxWireMap q (JuxQueryRequest q)
  , responses  :: JuxWireMap q (JuxQueryResponse q)
  , types      :: JuxTypes' e
  } deriving (Generic, Typeable)
-- deriving instance JuxStoreType e q => Show (JuxWire e q)
deriving instance JuxStoreType e q => ToJSON (JuxWire e q)
deriving instance JuxWireType e q => FromJSON (JuxWire e q)

juxWireToStore :: JuxStoreType e q => JuxWire e q -> JuxStore' e q
juxWireToStore (JuxWire a e q r t) = JuxStore a' e' q' r' t
  where
    a' = toIdMap . fold . HM.elems $ a
    e' = toIdMap e
    q' = toIdMap q
    r' = toIdMap r
    toIdMap :: (JuxLabel l) => JuxWireMap l v -> JuxIdMap l v Identity
    toIdMap = HM.fromList . concatMap (uncurry toIdPair) . HM.toList . getJuxWireMap
    toIdPair k = fmap (JuxId k *** Identity) . HM.toList

juxStoreToWire :: JuxStoreType e q => JuxStore' e q -> JuxWire e q
juxStoreToWire (JuxStore a e q r t) = JuxWire a' e' q' r' t
  where
    a' = HM.fromListWith (<>) . fmap toWireAttributePair . toWirePairs $ a
    e' = toWireMap e
    q' = toWireMap q
    r' = toWireMap r
    toWireMap :: (JuxLabel l) => JuxIdMap l v Identity -> JuxWireMap l v
    toWireMap = JuxWireMap . HM.fromList . toWirePairs
    toWirePairs = fmap (uncurry embedMap . toWirePair) . HM.toList
    toWirePair = (juxType &&& juxRawId) *** runIdentity
    embedMap (l, k) = (l,) . HM.singleton k
    toWireAttributePair = getJuxAttributeEntityType . fst &&& JuxWireMap . uncurry HM.singleton


-- TODO: put these not here
juxLabelToWire, juxWireToLabel :: String -> String
juxLabelToWire = toQuietSnake . fromHumps
juxWireToLabel = toPascal . fromSnake

showJuxLabel :: (Show a, IsString s) => a -> s
showJuxLabel = fromString . juxLabelToWire . show

readJuxLabel :: (Read a) => (Text -> e) -> Text -> Either e a
readJuxLabel toError t = case reads s of
  [(a,[])] -> Right a
  _        -> Left (toError t)
  where s = juxWireToLabel . T.unpack $ t

juxReadError :: Text -> Text -> String
juxReadError target source
  = "Could not read type "<> show target <>" from string "<> show source
