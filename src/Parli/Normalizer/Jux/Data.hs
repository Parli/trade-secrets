{-# LANGUAGE TemplateHaskell #-}
module Parli.Normalizer.Jux.Data where

import RIO

import           Parli.Jux
import qualified Parli.Normalizer.Types.Values as Value

data Entity
  = Alias                Value.Alias
  | ProductFamily        Value.ProductFamily
  | ProductGeneration    Value.ProductGeneration
  | ProductType          Value.ProductType
  | ProductConfiguration Value.ProductConfiguration
  | Crawl                Value.Crawl
  | Rating               Value.Rating
  deriving (Eq, Ord, Show, Read, Generic)
deriveJuxDataToJSON ''Entity
deriveJuxDataFromJSON ''Entity

data Attribute
  = ProductFamilyAliasIds         [JuxId]
  | ProductFamilyPriceObj         Value.Money
  | ProductFamilyRatingIds        [JuxId]
  | ProductGenerationAliasIds     [JuxId]
  | ProductGenerationPriceObj     Value.Money
  | ProductGenerationRatingIds    [JuxId]
  | ProductTypeAliasIds           [JuxId]
  | ProductTypePriceObj           Value.Money
  | ProductTypeRatingIds          [JuxId]
  | ProductConfigurationAliasIds  [JuxId]
  | ProductConfigurationPriceObj  Value.Money
  | ProductConfigurationRatingIds [JuxId]
  deriving (Eq, Ord, Show, Read, Generic)
deriveJuxDataToJSON ''Attribute
deriveJuxDataFromJSON ''Attribute
