{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Parli.Jux.Wire
( JuxWire, juxStoreToWire, juxWireToStore
) where

import           Data.Aeson
import           Parli.Jux.Types
import           RIO
import qualified RIO.HashMap as HM
import qualified RIO.HashSet as HS

type JuxWireMap l v = HashMap l (HashMap JuxRawId v)
data JuxWire e q = JuxWire
  { attributes :: HashMap e (JuxWireMap (JuxAttributeType e) (JuxAttributeData e))
  , entities   :: JuxWireMap e (JuxEntityData e)
  , queries    :: JuxWireMap q (JuxQueryRequest q)
  , responses  :: JuxWireMap q (JuxQueryResponse q)
  , types      :: HashMap JuxRawId e
  } deriving (Generic, Typeable)
deriving instance (JuxEntityType e, JuxQueryType q) => ToJSON (JuxWire e q)
deriving instance (JuxEntityType e, JuxQueryType q) => FromJSON (JuxWire e q)

juxStoreToWire :: (JuxEntityType e, JuxEntityType q)
  => JuxStore e q Identity -> JuxWire e q
juxStoreToWire (JuxStore a e q r t) = JuxWire a' e' q' r' t'
  where
    a' = undefined
    e' = undefined
    q' = undefined
    r' = undefined
    t' = undefined

juxWireToStore :: (JuxEntityType e, JuxEntityType q)
  => JuxWire e q -> JuxStore e q Identity
juxWireToStore (JuxWire a' e' q' r' t') = JuxStore a e q r t
  where
    a = toJuxIdMap . fold . HM.elems $ a'
    e = toJuxIdMap e'
    q = toJuxIdMap q'
    r = toJuxIdMap r'
    t = HS.fromList . fmap (uncurry $ flip JuxId) $ HM.toList t'
    toJuxIdMap :: (JuxLabel l) => JuxWireMap l v -> JuxIdMap l v Identity
    toJuxIdMap = HM.fromList . concatMap (uncurry toJuxId) . HM.toList
    toJuxId k = fmap (JuxId k *** Identity) . HM.toList
