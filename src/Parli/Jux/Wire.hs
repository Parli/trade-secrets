{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Parli.Jux.Wire
( JuxWire, juxStoreToWire, juxWireToStore
) where

import Data.Aeson
import Parli.Jux.Types
import RIO

type JuxRawIdMap a f = HashMap JuxRawId (f a)
type JuxWireObjectMap a b f = HashMap a (JuxRawIdMap b f)
type JuxWireAttributeMap a f
  = HashMap a (HashMap (JuxAttributeType a) (JuxRawIdMap (JuxAttributeData a) f))
type JuxWireConstraint e q f =
  ( Functor f, JuxEntityType e, JuxQueryType q
  , JuxValue (f (JuxEntityData e)), JuxValue (f (JuxAttributeData e))
  , JuxValue (f (JuxQueryRequest q)), JuxValue (f (JuxQueryResponse q))
  )
data JuxWire e q f = JuxWire
  { attributes :: JuxWireAttributeMap e f
  , entities   :: JuxWireObjectMap e (JuxEntityData e) f
  , queries    :: JuxWireObjectMap q (JuxQueryRequest q) f
  , responses  :: JuxWireObjectMap q (JuxQueryResponse q) f
  , types      :: HashMap JuxRawId e
  } deriving (Generic, Typeable)
deriving instance JuxWireConstraint e q f => ToJSON (JuxWire e q f)
deriving instance JuxWireConstraint e q f => FromJSON (JuxWire e q f)

juxStoreToWire :: JuxStore e q f -> JuxWire e q f
juxStoreToWire = undefined

juxWireToStore :: JuxWire e q f -> JuxStore e q f
juxWireToStore = undefined
