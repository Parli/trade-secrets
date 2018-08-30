{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
module Parli.Normalizer.Types.Values where

import           RIO
import qualified RIO.Partial as Unsafe

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
instance ToJSON Money where
  toJSON x = object
    [ "amount" .= show (moneyAmount x), "currency" .= moneyCurrency x ]
instance FromJSON Money where
  parseJSON = withObject "Money" $ \v ->
    Money <$> (Unsafe.read <$> v .: "amount") <*> v .: "currency"

data PriceIntent = PriceIntent
  { priceIntentIntent :: Text
  , priceIntentPrice  :: Money
  } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
deriveNormalizerObjectJSON ''PriceIntent

data Context = Context
  { contextMlCategoryIds :: HashSet Text
  , contextMlKeywordIds  :: HashSet Text
  , contextMlFilterIds   :: HashSet Text
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
  { aliasId   :: JuxId
  , aliasName :: Text
  } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
deriveNormalizerObjectJSON ''Alias

data ProductFamily = ProductFamily
  { productFamilyId   :: JuxId
  , productFamilyName :: Text
  } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
deriveNormalizerObjectJSON ''ProductFamily

data ProductGeneration = ProductGeneration
  { productGenerationId   :: JuxId
  , productGenerationName :: Text
  } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
deriveNormalizerObjectJSON ''ProductGeneration

data ProductType = ProductType
  { productTypeId   :: JuxId
  , productTypeName :: Text
  } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
deriveNormalizerObjectJSON ''ProductType

data ProductConfiguration = ProductConfiguration
  { productConfigurationId   :: JuxId
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
  { crawlId          :: JuxId
  , crawlSourceId    :: JuxId
  , crawlPageId      :: JuxId
  , crawlType        :: CrawlType
  , crawlUrl         :: Text
  , crawlAccessedAt  :: Epoch
  , crawlUpdatedAt   :: Maybe Epoch
  , crawlPublishedAt :: Maybe Epoch
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
  = RatingValueRank Rank
  | RatingValueScore Score
  | RatingValueText Text
  | RatingValueEmpty
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
instance ToJSON RatingValue where
  toJSON (RatingValueRank x)  = toJSON x
  toJSON (RatingValueScore x) = toJSON x
  toJSON (RatingValueText x)  = toJSON x
  toJSON _                    = toJSON True
instance FromJSON RatingValue where
  parseJSON v
    =   RatingValueRank <$> parseJSON v
    <|> RatingValueScore <$> parseJSON v
    <|> RatingValueText <$> parseJSON v
    <|> pure RatingValueEmpty

data Rating = Rating
  { ratingId          :: JuxId
  , ratingCrawlId     :: JuxId
  , ratingType        :: RatingType
  , ratingValue       :: RatingValue
  , ratingContext     :: Context
  , ratingPriceIntent :: PriceIntent
  } deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)
deriveNormalizerObjectJSON ''Rating

-- Question analysis route response
data Analysis = Analysis
  { analysisContext     :: Context
  , analysisPriceIntent :: PriceIntent
  } deriving (Show)
deriveNormalizerObjectJSON ''Analysis

-- Convenience
emptyAnalysis :: Analysis
emptyAnalysis = Analysis mempty emptyPriceIntent

emptyPriceIntent :: PriceIntent
emptyPriceIntent = PriceIntent "none" emptyMoney

emptyMoney :: Money
emptyMoney = Money 0 XXX
