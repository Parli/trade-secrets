module Parli.Jux
( module Parli.Jux.Types
, module Parli.Jux.TH
, module Parli.Jux
) where

import           RIO
import qualified RIO.HashMap as HM

import Data.Aeson.TH
import Data.Aeson.Types
import Language.Haskell.TH

import Parli.Jux.TH
import Parli.Jux.Types

-- convenient constructors
juxStorable :: (Hashable a) => JuxId a -> b -> JuxIdMap a b Identity
juxStorable k = HM.singleton k . Identity

storeAttributes :: JuxStoreType e q => JuxAttributes' e Identity -> JuxStore' e q
storeAttributes xs = mempty { juxAttributes = xs }
storeEntities :: JuxStoreType e q => JuxEntities' e Identity -> JuxStore' e q
storeEntities xs = mempty { juxEntities = xs }
storeQueries :: JuxStoreType e q => JuxQueries' q Identity -> JuxStore' e q
storeQueries xs = mempty { juxQueries = xs }
storeResponses :: JuxStoreType e q => JuxResponses' q Identity -> JuxStore' e q
storeResponses xs = mempty { juxResponses = xs }

juxStorableType :: (Hashable e) => JuxId e -> HashMap Text e
juxStorableType = HM.singleton . juxRawId <*> juxType

storeTypes :: JuxStoreType e q => JuxTypes' e -> JuxStore' e q
storeTypes xs = mempty { juxTypes = xs }

-- constrained morphisms
juxMorph :: (JuxId a -> f b -> g c) -> JuxIdMap a b f -> JuxIdMap a c g
juxMorph = HM.mapWithKey

juxTraverse :: (Monad m)
  => (JuxId a -> f b -> m (g c)) -> JuxIdMap a b f -> m (JuxIdMap a c g)
juxTraverse action = sequence . HM.mapWithKey action

juxResolve :: JuxIdMap a b Maybe -> JuxIdMap a b Identity
juxResolve = HM.mapMaybe (fmap Identity)

toAttributeId :: (a ~ JuxAttributeType e) => a -> JuxId e -> JuxId a
toAttributeId a k = k { juxType = a }

toEntityId :: (JuxEntityType e, a ~ JuxAttributeType e) => JuxId a -> JuxId e
toEntityId k@JuxId{ juxType = a }
  = k{ juxType = getJuxAttributeEntityType a }

-- (de)serialization
juxToJSONKey :: (JuxLabel a) => ToJSONKeyFunction a
juxToJSONKey = toJSONKeyText showJuxLabel

juxFromJSONKey :: (JuxLabel a) => Text -> FromJSONKeyFunction a
juxFromJSONKey target
  = FromJSONKeyText $ either error id . readJuxLabel (juxReadError target)

deriveJuxLabelJSON :: Name -> DecsQ
deriveJuxLabelJSON = deriveJSON defaultOptions
  { constructorTagModifier = juxLabelToWire
  , tagSingleConstructors = True
  }
