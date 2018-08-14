-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Parli.Jux.Types where

import           Data.Aeson
import           RIO
import qualified RIO.HashMap as HM
import qualified RIO.HashSet as HS

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
  hasJuxEntityData          :: JuxEntityData a -> Bool
  -- hasJuxAttributeData       :: JuxAttributeData a -> Bool

class
  ( JuxLabel a
  , JuxValue (JuxQueryRequest a), JuxValue (JuxQueryResponse a)
  ) => JuxQueryType a where
  type JuxQueryRequest  a = x | x -> a
  type JuxQueryResponse a = y | y -> a
  getJuxRequestQuery  :: JuxQueryRequest a -> a
  getJuxResponseQuery :: JuxQueryResponse a -> a

-- newtype Marker a = Marker { getMarker :: Proxy a } -- Is this ever needed?
--   deriving (Eq, Ord, Show, Functor, Generic)
--   deriving newtype (ToJSON, FromJSON)

-- newtype Content a = Content { getContent :: a }
--   deriving (Eq, Ord, Show, Functor, Generic)
--   deriving newtype (ToJSON, FromJSON)


type JuxRawId = Text
type JuxIdMap a b f = HashMap (JuxId a) (f b)

data JuxId a = JuxId
  { juxType :: a
  , juxId   :: JuxRawId
  } deriving (Eq, Ord, Show, Data, Typeable, Generic, Hashable)

data JuxStore e q f = JuxStore
  { juxAttributes :: JuxIdMap (JuxAttributeType e) (JuxAttributeData e) f
  , juxEntities   :: JuxIdMap e (JuxEntityData e) f
  , juxQueries    :: JuxIdMap q (JuxQueryRequest q) f
  , juxResponses  :: JuxIdMap q (JuxQueryResponse q) f
  , juxTypes      :: HashSet (JuxId e)
  } deriving (Generic, Typeable)
instance (JuxEntityType e, JuxEntityType q) => Semigroup (JuxStore e q f) where
  (JuxStore a1 e1 q1 r1 t1) <> (JuxStore a2 e2 q2 r2 t2)
    = JuxStore -- favor keys in right-hand argument
      (HM.union a2 a1) (HM.union e2 e1)
      (HM.union q2 q1) (HM.union r2 r1)
      (HS.union t2 t1)
instance (JuxEntityType e, JuxEntityType q) => Monoid (JuxStore e q f) where
  mempty = JuxStore mempty mempty mempty mempty mempty
  mappend = (<>)
