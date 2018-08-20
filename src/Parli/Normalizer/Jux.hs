{-# OPTIONS_GHC -fno-warn-orphans #-}
module Parli.Normalizer.Jux
( module Parli.Normalizer.Jux
, module Parli.Normalizer.Types.Values
) where

import           Parli.Jux.Types
import qualified Parli.Normalizer.Jux.Data as Data
import qualified Parli.Normalizer.Jux.Types as Type
import           Parli.Normalizer.Types.Values

type JuxAttributes f = JuxAttributes' Type.Entity f
type JuxEntities f = JuxEntities' Type.Entity f
type JuxQueries f = JuxQueries' Type.Query f
type JuxResponses f = JuxResponses' Type.Query f
type JuxTypes = JuxTypes' Type.Entity
type JuxStore = JuxStore' Type.Entity Type.Query

instance JuxEntityType Type.Entity where
  type JuxAttributeType Type.Entity = Type.Attribute
  type JuxEntityData    Type.Entity = Data.Entity
  type JuxAttributeData Type.Entity = Data.Attribute
  getJuxDataEntityType = \case
    Data.Alias                _ -> Type.Alias
    Data.ProductFamily        _ -> Type.ProductFamily
    Data.ProductGeneration    _ -> Type.ProductGeneration
    Data.ProductType          _ -> Type.ProductType
    Data.ProductConfiguration _ -> Type.ProductConfiguration
    Data.Crawl                _ -> Type.Crawl
    Data.Rating               _ -> Type.Rating
  getJuxDataAttributeType = \case
    Data.ProductFamilyAliasIds         _ -> Type.ProductFamilyAliasIds
    Data.ProductFamilyPriceObj         _ -> Type.ProductFamilyPriceObj
    Data.ProductFamilyRatingIds        _ -> Type.ProductFamilyRatingIds
    Data.ProductGenerationAliasIds     _ -> Type.ProductGenerationAliasIds
    Data.ProductGenerationPriceObj     _ -> Type.ProductGenerationPriceObj
    Data.ProductGenerationRatingIds    _ -> Type.ProductGenerationRatingIds
    Data.ProductTypeAliasIds           _ -> Type.ProductTypeAliasIds
    Data.ProductTypePriceObj           _ -> Type.ProductTypePriceObj
    Data.ProductTypeRatingIds          _ -> Type.ProductTypeRatingIds
    Data.ProductConfigurationAliasIds  _ -> Type.ProductConfigurationAliasIds
    Data.ProductConfigurationPriceObj  _ -> Type.ProductConfigurationPriceObj
    Data.ProductConfigurationRatingIds _ -> Type.ProductConfigurationRatingIds
  getJuxAttributeEntityType = \case
    Type.ProductFamilyAliasIds         -> Type.ProductFamily
    Type.ProductFamilyPriceObj         -> Type.ProductFamily
    Type.ProductFamilyRatingIds        -> Type.ProductFamily
    Type.ProductGenerationAliasIds     -> Type.ProductGeneration
    Type.ProductGenerationPriceObj     -> Type.ProductGeneration
    Type.ProductGenerationRatingIds    -> Type.ProductGeneration
    Type.ProductTypeAliasIds           -> Type.ProductType
    Type.ProductTypePriceObj           -> Type.ProductType
    Type.ProductTypeRatingIds          -> Type.ProductType
    Type.ProductConfigurationAliasIds  -> Type.ProductConfiguration
    Type.ProductConfigurationPriceObj  -> Type.ProductConfiguration
    Type.ProductConfigurationRatingIds -> Type.ProductConfiguration
