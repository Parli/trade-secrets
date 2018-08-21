{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
module Parli.Normalizer.Types.Values where

import RIO

import Data.Aeson
import Data.Currency
import Parli.Jux
import Parli.Normalizer.Types.Values.Aeson

-- Shared
type Epoch = Int

data Money = Money
  { moneyAmount   :: Int
  , moneyCurrency :: Alpha
  } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
deriveNormalizerObjectJSON ''Money

data PriceIntent = PriceIntent
  { priceIntentIntent :: Text
  , priceIntentPrice  :: Money
  } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
deriveNormalizerObjectJSON ''PriceIntent

data Context = Context
  { contextCategories :: HashSet Text
  , contextKeywords   :: HashSet Text
  , contextFilters    :: HashSet Text
  } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
deriveNormalizerObjectJSON ''Context
instance Semigroup Context where
  (Context cs ks fs) <> (Context cs' ks' fs')
    = Context (cs<>cs') (ks<>ks') (fs<>fs')
instance Monoid Context where
  mempty = Context mempty mempty mempty
  mappend = (<>)

-- Entities: Product
data Alias = Alias
  { aliasId   :: JuxRawId
  , aliasName :: Text
  } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
deriveNormalizerObjectJSON ''Alias

data ProductFamily = ProductFamily
  { productFamilyId   :: JuxRawId
  , productFamilyName :: Text
  } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
deriveNormalizerObjectJSON ''ProductFamily

data ProductGeneration = ProductGeneration
  { productGenerationId   :: JuxRawId
  , productGenerationName :: Text
  } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
deriveNormalizerObjectJSON ''ProductGeneration

data ProductType = ProductType
  { productTypeId   :: JuxRawId
  , productTypeName :: Text
  } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
deriveNormalizerObjectJSON ''ProductType

data ProductConfiguration = ProductConfiguration
  { productConfigurationId   :: JuxRawId
  , productConfigurationName :: Text
  } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
deriveNormalizerObjectJSON ''ProductConfiguration

-- Entities: Crawl
data CrawlType
  = CrawlTypeUnknown Text
  | CrawlTypeReview
  | CrawlTypeRoundup
  | CrawlTypeNews
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
instance ToJSON CrawlType where
  toJSON = normalizerEnumToJSON ''CrawlType
instance FromJSON CrawlType where
  parseJSON = normalizerEnumParseJSON CrawlTypeUnknown ''CrawlType

data Crawl = Crawl
  { crawlId        :: JuxRawId
  , crawlSourceId  :: JuxRawId
  , crawlPageId    :: JuxRawId
  , crawlType      :: CrawlType
  , crawlUrl       :: Text
  , crawlAccessed  :: Epoch
  , crawlUpdated   :: Maybe Epoch
  , crawlPublished :: Maybe Epoch
  } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
deriveNormalizerObjectJSON ''Crawl

-- Entities: Rating
data RatingType
  = RatingTypeUnknown Text
  | RatingTypeRank
  | RatingTypeOnList
  | RatingTypeTopPick
  | RatingTypeRunnerUp
  | RatingTypeScore
  | RatingTypeSubscore
  | RatingTypeAttributeScore
  | RatingTypeConsidered
  | RatingTypeMentioned
  | RatingTypeAward
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
instance ToJSON RatingType where
  toJSON = normalizerEnumToJSON ''RatingType
instance FromJSON RatingType where
  parseJSON = normalizerEnumParseJSON RatingTypeUnknown ''RatingType

data Rank = Rank
  { rankPosition :: Int
  , rankOutOf    :: Int
  } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
deriveNormalizerObjectJSON ''Rank

data Score = Score
  { scoreRating :: Double
  , scoreMax    :: Double
  } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
deriveNormalizerObjectJSON ''Score

data RatingValue
  = RatingRank Rank
  | RatingScore Score
  | RatingEmpty
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
instance ToJSON RatingValue where
  toJSON (RatingRank x)  = toJSON x
  toJSON (RatingScore x) = toJSON x
  toJSON _               = Null
instance FromJSON RatingValue where
  parseJSON v
    =   RatingRank <$> parseJSON v
    <|> RatingScore <$> parseJSON v
    <|> pure RatingEmpty

data Rating = Rating
  { ratingId          :: JuxRawId
  , ratingType        :: RatingType
  , ratingValue       :: RatingValue
  , ratingContext     :: Context
  , ratingPriceIntent :: PriceIntent
  } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
deriveNormalizerObjectJSON ''Rating
