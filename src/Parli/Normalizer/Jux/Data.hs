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
  = ProductFamilyAliasIds         [JuxRawId]
  | ProductFamilyPriceObj         Value.Money
  | ProductFamilyRatingIds        [JuxRawId]
  | ProductGenerationAliasIds     [JuxRawId]
  | ProductGenerationPriceObj     Value.Money
  | ProductGenerationRatingIds    [JuxRawId]
  | ProductTypeAliasIds           [JuxRawId]
  | ProductTypePriceObj           Value.Money
  | ProductTypeRatingIds          [JuxRawId]
  | ProductConfigurationAliasIds  [JuxRawId]
  | ProductConfigurationPriceObj  Value.Money
  | ProductConfigurationRatingIds [JuxRawId]
  deriving (Eq, Ord, Show, Read, Generic)
deriveJuxDataToJSON ''Attribute
deriveJuxDataFromJSON ''Attribute
