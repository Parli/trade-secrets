{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Parli.Jux.Wire
( JuxWire, juxStoreToWire, juxWireToStore
) where

import           Data.Aeson
import           Parli.Jux.Types
import           RIO
import qualified RIO.HashMap as HM

type JuxWireMap l v = HashMap l (HashMap JuxRawId v)
data JuxWire e q = JuxWire
  { attributes :: HashMap e (JuxWireMap (JuxAttributeType e) (JuxAttributeData e))
  , entities   :: JuxWireMap e (JuxEntityData e)
  , queries    :: JuxWireMap q (JuxQueryRequest q)
  , responses  :: JuxWireMap q (JuxQueryResponse q)
  , types      :: JuxTypeMap e
  } deriving (Generic, Typeable)
deriving instance (JuxEntityType e, JuxQueryType q) => ToJSON (JuxWire e q)
deriving instance (JuxEntityType e, JuxQueryType q) => FromJSON (JuxWire e q)

juxStoreToWire :: (JuxEntityType e, JuxEntityType q) => JuxStore e q -> JuxWire e q
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

juxWireToStore :: (JuxEntityType e, JuxEntityType q) => JuxWire e q -> JuxStore e q
juxWireToStore (JuxWire a e q r t) = JuxStore a' e' q' r' t
  where
    a' = toIdMap . fold . HM.elems $ a
    e' = toIdMap e
    q' = toIdMap q
    r' = toIdMap r
    toIdMap :: (JuxLabel l) => JuxWireMap l v -> JuxIdMap l v Identity
    toIdMap = HM.fromList . concatMap (uncurry toIdPair) . HM.toList
    toIdPair k = fmap (JuxId k *** Identity) . HM.toList
