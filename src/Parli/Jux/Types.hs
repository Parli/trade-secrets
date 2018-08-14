{-# LANGUAGE TypeFamilyDependencies #-}
module Parli.Jux.Types where

import           Data.Aeson
import           RIO
import qualified RIO.HashMap as HM

type JuxValue a = (Eq a, Show a, ToJSON a, FromJSON a)
type JuxLabel a = (JuxValue a, Hashable a, ToJSONKey a, FromJSONKey a)

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
type JuxIdMap a b f = HashMap (JuxId a) (f b)
type JuxTypeMap a = HashMap JuxRawId a

data JuxId a = JuxId
  { juxType  :: a
  , juxRawId :: JuxRawId
  } deriving (Eq, Ord, Show, Data, Typeable, Generic, Hashable)

data JuxStore e q = JuxStore
  { juxAttributes :: JuxIdMap (JuxAttributeType e) (JuxAttributeData e) Identity
  , juxEntities   :: JuxIdMap e (JuxEntityData e) Identity
  , juxQueries    :: JuxIdMap q (JuxQueryRequest q) Identity
  , juxResponses  :: JuxIdMap q (JuxQueryResponse q) Identity
  , juxTypes      :: JuxTypeMap e
  } deriving (Generic, Typeable)
instance (JuxEntityType e, JuxQueryType q) => Semigroup (JuxStore e q) where
  (JuxStore a1 e1 q1 r1 t1) <> (JuxStore a2 e2 q2 r2 t2)
    = JuxStore -- favor keys in right-hand argument
      (HM.union a2 a1) (HM.union e2 e1)
      (HM.union q2 q1) (HM.union r2 r1)
      (HM.union t2 t1)
instance (JuxEntityType e, JuxQueryType q) => Monoid (JuxStore e q) where
  mempty = JuxStore mempty mempty mempty mempty mempty
  mappend = (<>)

attributesStore :: (JuxEntityType e, JuxQueryType q)
  => JuxIdMap (JuxAttributeType e) (JuxAttributeData e) Identity -> JuxStore e q
attributesStore xs = mempty { juxAttributes = xs }

entitiesStore :: (JuxEntityType e, JuxQueryType q)
  => JuxIdMap e (JuxEntityData e) Identity -> JuxStore e q
entitiesStore xs = mempty { juxEntities = xs }

queriesStore :: (JuxEntityType e, JuxQueryType q)
  => JuxIdMap q (JuxQueryRequest q) Identity -> JuxStore e q
queriesStore xs = mempty { juxQueries = xs }

responsesStore :: (JuxEntityType e, JuxQueryType q)
  => JuxIdMap q (JuxQueryResponse q) Identity -> JuxStore e q
responsesStore xs = mempty { juxResponses = xs }

typesStore :: (JuxEntityType e, JuxQueryType q)
  => JuxTypeMap e -> JuxStore e q
typesStore xs = mempty { juxTypes = xs }
