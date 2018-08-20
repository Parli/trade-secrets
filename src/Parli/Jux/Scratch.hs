{-# LANGUAGE TemplateHaskell #-}
module Parli.Jux.Scratch where

import Data.Aeson
import Parli.Jux
import RIO

data FooE = FooE
  deriving (Eq, Show, Read, Generic, Hashable)
deriveJuxLabelJSON ''FooE
instance ToJSONKey FooE where toJSONKey = juxToJSONKey
instance FromJSONKey FooE where fromJSONKey = juxFromJSONKey "FooE"
data FooA = FooA
  deriving (Eq, Show, Read, Generic, Hashable)
deriveJuxLabelJSON ''FooA
instance ToJSONKey FooA where toJSONKey = juxToJSONKey
instance FromJSONKey FooA where fromJSONKey = juxFromJSONKey "FooA"
data FooED = FooED { fooEntityValue :: () }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
data FooAD = FooAD { fooAttributeValue :: () }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
instance JuxEntityType FooE where
  type JuxAttributeType FooE = FooA
  type JuxEntityData FooE    = FooED
  type JuxAttributeData FooE = FooAD
  getJuxAttributeEntityType = const FooE
  getJuxDataEntityType      = const FooE
  getJuxDataAttributeType   = const FooA

data FooQ = FooQ
  deriving (Eq, Show, Read, Generic, Hashable)
deriveJuxLabelJSON ''FooQ
instance ToJSONKey FooQ where toJSONKey = juxToJSONKey
instance FromJSONKey FooQ where fromJSONKey = juxFromJSONKey "FooQ"
data FooQRq = FooQRq { fooRequestValue :: () }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
data FooQRs = FooQRs { fooResponseValue :: () }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
instance JuxQueryType FooQ where
  type JuxQueryRequest FooQ = FooQRq
  type JuxQueryResponse FooQ = FooQRs
  getJuxRequestQuery = const FooQ
  getJuxResponseQuery = const FooQ

type JuxAttributes f = JuxAttributes' FooE f
type JuxEntities f = JuxEntities' FooE f
type JuxQueries f = JuxQueries' FooQ f
type JuxResponses f = JuxResponses' FooQ f
type JuxTypes = JuxTypes' FooE
type JuxStore = JuxStore' FooE FooQ

-- Test
fooEId :: JuxId FooE
fooEId = JuxId FooE "1"
fooAId :: JuxId FooA
fooAId = toAttributeId FooA fooEId
fooQId :: JuxId FooQ
fooQId = JuxId FooQ "2"

fooAttributes :: JuxStore
fooAttributes = storeAttributes . juxStorable fooAId $ FooAD ()
fooEntities :: JuxStore
fooEntities = storeEntities . juxStorable fooEId $ FooED ()
fooQueries :: JuxStore
fooQueries = storeQueries . juxStorable fooQId $ FooQRq ()
fooResponses :: JuxStore
fooResponses = storeResponses . juxStorable fooQId $ FooQRs ()
fooTypes :: JuxStore
fooTypes = storeTypes $ juxStorableType fooEId

fooStore :: JuxStore
fooStore = fooAttributes <> fooEntities <> fooQueries <> fooResponses <> fooTypes
fooEncoded :: LByteString
fooEncoded = encode fooStore
fooRoundtrip :: JuxStore
fooRoundtrip = fromMaybe mempty $ decode fooEncoded

-- λ. fooEncoded
-- "{\"queries\":{\"foo_q\":{\"2\":{\"fooRequestValue\":[]}}},\"entities\":{\"foo_e\":{\"1\":{\"fooEntityValue\":[]}}},\"responses\":{\"foo_q\":{\"2\":{\"fooResponseValue\":[]}}},\"types\":{\"1\":\"foo_e\"},\"attributes\":{\"foo_e\":{\"foo_a\":{\"1\":{\"fooAttributeValue\":[]}}}}}"
-- λ. fooRoundtrip
-- JuxStore {juxAttributes = fromList [(JuxId {juxType = FooA, juxRawId = "1"},Identity (FooAD {fooAttributeValue = ()}))], juxEntities = fromList [(JuxId {juxType = FooE, juxRawId = "1"},Identity (FooED {fooEntityValue = ()}))], juxQueries = fromList [(JuxId {juxType = FooQ, juxRawId = "2"},Identity (FooQRq {fooRequestValue = ()}))], juxResponses = fromList [(JuxId {juxType = FooQ, juxRawId = "2"},Identity (FooQRs {fooResponseValue = ()}))], juxTypes = fromList [("1",FooE)]}
-- λ. fooStore == fooRoundtrip
-- True
