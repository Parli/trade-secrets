{-# LANGUAGE TemplateHaskell #-}
module Parli.Normalizer.Jux.Types where

import RIO

import Data.Aeson
import Parli.Jux
import Parli.Jux.Vacant

data Entity
  = Alias
  | ProductFamily
  | ProductGeneration
  | ProductType
  | ProductConfiguration
  | Crawl
  | Rating
  deriving (Eq, Ord, Show, Read, Generic, Hashable)
deriveJuxLabelJSON ''Entity
instance ToJSONKey Entity where toJSONKey = juxToJSONKey
instance FromJSONKey Entity where fromJSONKey = juxFromJSONKey "Entity"

data Attribute
  = ProductFamilyAliasIds
  | ProductFamilyPriceObj
  | ProductFamilyRatingIds
  | ProductGenerationAliasIds
  | ProductGenerationPriceObj
  | ProductGenerationRatingIds
  | ProductTypeAliasIds
  | ProductTypePriceObj
  | ProductTypeRatingIds
  | ProductConfigurationAliasIds
  | ProductConfigurationPriceObj
  | ProductConfigurationRatingIds
  deriving (Eq, Ord, Show, Read, Generic, Hashable)
deriveJuxLabelJSON ''Attribute
instance ToJSONKey Attribute where toJSONKey = juxToJSONKey
instance FromJSONKey Attribute where fromJSONKey = juxFromJSONKey "Attribute"

type Query = JuxVacantType
