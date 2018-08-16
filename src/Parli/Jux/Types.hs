{-# LANGUAGE TypeFamilyDependencies #-}
module Parli.Jux.Types where

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

type JuxRawId = Text
data JuxId a = JuxId
  { juxType  :: a
  , juxRawId :: JuxRawId
  } deriving (Eq, Ord, Show, Data, Typeable, Generic, Hashable)

type JuxIdMap a b f = HashMap (JuxId a) (f b)
type JuxAttributes e f = JuxIdMap (JuxAttributeType e) (JuxAttributeData e) f
type JuxEntities e f = JuxIdMap e (JuxEntityData e) f
type JuxQueries q f = JuxIdMap q (JuxQueryRequest q) f
type JuxResponses q f = JuxIdMap q (JuxQueryResponse q) f
type JuxTypes a = HashMap JuxRawId a

type JuxStoreType e q = (JuxEntityType e, JuxQueryType q)
data JuxStore e q = JuxStore
  { juxAttributes :: JuxAttributes e Identity
  , juxEntities   :: JuxEntities e Identity
  , juxQueries    :: JuxQueries q Identity
  , juxResponses  :: JuxResponses q Identity
  , juxTypes      :: JuxTypes e
  } deriving (Generic, Typeable)
deriving instance JuxStoreType e q => Show (JuxStore e q)
instance JuxStoreType e q => Semigroup (JuxStore e q) where
  (JuxStore a1 e1 q1 r1 t1) <> (JuxStore a2 e2 q2 r2 t2)
    = JuxStore -- favor keys in right-hand argument
      (HM.union a2 a1) (HM.union e2 e1)
      (HM.union q2 q1) (HM.union r2 r1)
      (HM.union t2 t1)
instance JuxStoreType e q => Monoid (JuxStore e q) where
  mempty = JuxStore mempty mempty mempty mempty mempty
  mappend = (<>)

type JuxWireMap l v = HashMap l (HashMap JuxRawId v)
data JuxWire e q = JuxWire
  { attributes :: HashMap e (JuxWireMap (JuxAttributeType e) (JuxAttributeData e))
  , entities   :: JuxWireMap e (JuxEntityData e)
  , queries    :: JuxWireMap q (JuxQueryRequest q)
  , responses  :: JuxWireMap q (JuxQueryResponse q)
  , types      :: JuxTypes e
  } deriving (Generic, Typeable)
deriving instance JuxStoreType e q => Show (JuxWire e q)
deriving instance JuxStoreType e q => ToJSON (JuxWire e q)
deriving instance JuxStoreType e q => FromJSON (JuxWire e q)
