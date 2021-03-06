{-# LANGUAGE TemplateHaskell #-}
module Parli.Jux.Vacant where

import RIO

import Codec.Serialise
import Data.Aeson
import Parli.Jux.Core

data JuxVacantType = JuxVacantType
  deriving (Eq, Ord, Show, Read, Generic, Hashable, NFData, Serialise)
deriveJuxLabelJSON ''JuxVacantType
instance ToJSONKey JuxVacantType where
  toJSONKey = juxToJSONKey
instance FromJSONKey JuxVacantType where
  fromJSONKey = juxFromJSONKey "JuxVacantType"
instance JuxEntityType JuxVacantType where
  type JuxAttributeType JuxVacantType = JuxVacantType
  type JuxEntityData JuxVacantType = JuxVacantType
  type JuxAttributeData JuxVacantType = JuxVacantType
  getJuxAttributeEntityType = const JuxVacantType
  getJuxDataEntityType = const JuxVacantType
  getJuxDataAttributeType = const JuxVacantType
instance JuxQueryType JuxVacantType where
  type JuxQueryRequest JuxVacantType = JuxVacantType
  type JuxQueryResponse JuxVacantType = JuxVacantType
  getJuxRequestQuery = const JuxVacantType
  getJuxResponseQuery = const JuxVacantType

instance JuxLabelValue JuxVacantType JuxVacantType where
  juxLabelValueParseJSON _ _ = pure JuxVacantType
