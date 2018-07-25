module Parli.Normalizer.Types.Response
( Response(..)
, Analysis(..), emptyAnalysis
, Entities(..), emptyEntities
, Context(..), emptyContext
, PriceIntent(..), emptyPriceIntent
, Currency(..), emptyCurrency
, Crawl(..)
, CrawlType(..)
, Product(..), emptyProduct
, Rating(..)
, RatingType(..)
, RatingValue(..)
, Rank(..)
, Score(..)
) where

import RIO

import Data.Aeson
import Data.Aeson.Types
import Data.Monoid (Alt(..))

data Response = Response
  { responseAnalysis :: Analysis
  , responseEntities :: Entities
  } deriving (Show)
instance FromJSON Response where
  parseJSON = withObject "response" $ \v -> Response
    <$> v .:? "analysis" .!= emptyAnalysis
    <*> v .:? "entities" .!= emptyEntities

data Analysis = Analysis
  { analysisContext :: Context
  , analysisIntent  :: PriceIntent
  } deriving (Show)
instance FromJSON Analysis where
  parseJSON = withObject "analysis" $ \v -> Analysis
    <$> v .: "context"
    <*> v .: "price_intent"

data Context = Context
  { contextCategories :: HashSet Text
  , contextKeywords   :: HashSet Text
  , contextFilters    :: HashSet Text
  } deriving (Show)
instance FromJSON Context where
  parseJSON = withObject "context" $ \v -> Context
    <$> v .: "categories"
    <*> v .: "keywords"
    <*> v .: "filters"
instance Semigroup Context where
  (Context cs ks fs) <> (Context cs' ks' fs')
    = Context (cs<>cs') (ks<>ks') (fs<>fs')
instance Monoid Context where
  mempty = Context mempty mempty mempty
  mappend = (<>)

data PriceIntent = PriceIntent
  { intentAmount :: Currency
  , intentType   :: Text
  } deriving (Show)
instance FromJSON PriceIntent where
  parseJSON = withObject "intent" $ \v -> PriceIntent
    <$> v .: "price"
    <*> v .: "intent"

data Currency = Currency
  { currencyCode   :: Text
  , currencyAmount :: Int
  } deriving (Show)
instance FromJSON Currency where
  parseJSON = withObject "price" $ \v -> do
    code <- v .: "currency"
    amount <- maybe 0 id . readMaybe <$> v .: "amount"
    pure $ Currency code amount

data Entities = Entities
  { entitiesProducts :: Map Text Product
  , entitiesRatings  :: Map Text Rating
  , entitiesCrawls   :: Map Text Crawl
  } deriving (Show)
instance FromJSON Entities where
  parseJSON = withObject "entities" $ \v -> Entities
    <$> v .:? "products" .!= mempty
    <*> v .:? "ratings"  .!= mempty
    <*> v .:? "crawls"   .!= mempty
instance Semigroup Entities where
  (Entities ps rs gs) <> (Entities ps' rs' gs')
    = Entities (ps<>ps') (rs<>rs') (gs<>gs')
instance Monoid Entities where
  mempty = Entities mempty mempty mempty
  mappend = (<>)

data Crawl = Crawl
  { crawlId        :: Text
  , crawlPageId    :: Text
  , crawlSourceId  :: Text
  , crawlUrl       :: Text
  , crawlType      :: CrawlType
  , crawlAccessed  :: Int
  , crawlPublished :: Maybe Int
  , crawlUpdated   :: Maybe Int
  , crawlEarliest  :: Int
  } deriving (Show)
instance FromJSON Crawl where
  parseJSON = withObject "crawls" $ \v -> do
    id'        <- v .: "id"
    pageId'    <- v .: "page_id"
    sourceId'  <- v .: "source_id"
    url'       <- v .: "url"
    type'      <- v .: "type"
    accessed'  <- v .: "accessed_at"
    published' <- v .:? "published_at"
    updated'   <- v .:? "updated_at"
    let
      earliestish = getAlt $ Alt published' <> Alt updated'
      earliest' = maybe accessed' id earliestish
    pure $ Crawl id' pageId' sourceId' url' type' accessed' published' updated' earliest'

data CrawlType
  = CrawlTypeReview
  | CrawlTypeRoundup
  | CrawlTypeNews
  | CrawlTypeUnknown
  deriving (Eq, Ord, Show, Generic, Hashable)
instance FromJSON CrawlType where
  parseJSON = withText "crawl_type" $ \v -> pure $ case v of
    "review"  -> CrawlTypeReview
    "roundup" -> CrawlTypeRoundup
    "news"    -> CrawlTypeNews
    _         -> CrawlTypeUnknown

data Product = Product
  { productId      :: Text
  , productName    :: Text
  , productPrice   :: Currency
  , productContext :: Context
  } deriving (Show)
instance FromJSON Product where
  parseJSON = withObject "product" $ \v -> Product
    <$> v .: "id"
    <*> v .: "name"
    <*> v .: "price"
    <*> v .: "context"

data Rating = Rating
  { ratingId        :: Text
  , ratingCrawlId   :: Text
  , ratingProductId :: Text
  , ratingType      :: RatingType
  , ratingContext   :: Context
  , ratingIntent    :: PriceIntent
  , ratingValue     :: Maybe RatingValue
  } deriving (Show)
instance FromJSON Rating where
  parseJSON = withObject "ratings" $ \v -> do
    id'        <- v .: "id"
    crawlId'   <- v .: "crawl_id"
    productId' <- v .: "product_id"
    type'      <- v .: "type"
    context'   <- v .: "context"
    intent'    <- v .: "price_intent"
    value'     <- parseRatingValue type' =<< v .:? "value" .!= Null
    pure $ Rating id' crawlId' productId' type' context' intent' value'

data RatingType
  = RatingTypeRank
  | RatingTypeOnList
  | RatingTypeTopPick
  | RatingTypeRunnerUp
  | RatingTypeScore
  | RatingTypeSubscore
  | RatingTypeAttributeScore
  | RatingTypeConsidered
  | RatingTypeMentioned
  | RatingTypeAward
  | RatingTypeUnclassified
  deriving (Eq, Ord, Show, Generic, Hashable)
instance FromJSON RatingType where
  parseJSON = withText "rating_type" $ \v -> pure $ case v of
    "rank"            -> RatingTypeRank
    "on_list"         -> RatingTypeOnList
    "score"           -> RatingTypeScore
    "subscore"        -> RatingTypeSubscore
    "attribute_score" -> RatingTypeAttributeScore
    "top_pick"        -> RatingTypeTopPick
    "runner_up"       -> RatingTypeRunnerUp
    "considered"      -> RatingTypeConsidered
    "mentioned"       -> RatingTypeMentioned
    "award"           -> RatingTypeAward
    _                 -> RatingTypeUnclassified

data RatingValue
  = RatingValueRank  Rank
  | RatingValueScore Score
  deriving (Show)
parseRatingValue :: RatingType -> Value -> Parser (Maybe RatingValue)
parseRatingValue t (Object v)
  | elem t [RatingTypeRank, RatingTypeOnList]
  = Just . RatingValueRank <$> parseRank v
  | elem t [RatingTypeScore, RatingTypeSubscore, RatingTypeAttributeScore]
  = Just . RatingValueScore <$> parseScore v
parseRatingValue _ _
  = pure Nothing

data Rank = Rank
  { rankPosition :: Int
  , rankOutOf    :: Int
  } deriving (Show)
instance FromJSON Rank where
  parseJSON = withObject "rank" parseRank
parseRank :: Object -> Parser Rank
parseRank v = Rank
  <$> v .: "position"
  <*> v .: "out_of"

data Score = Score
  { scoreValue :: Double
  , scoreMax   :: Double
  } deriving (Show)
instance FromJSON Score where
  parseJSON = withObject "score" parseScore
parseScore :: Object -> Parser Score
parseScore v = Score
  <$> v .:  "rating"
  <*> v .:  "max"

emptyContext :: Context
emptyContext = mempty

emptyAnalysis :: Analysis
emptyAnalysis = Analysis emptyContext emptyPriceIntent

emptyEntities :: Entities
emptyEntities = mempty

emptyPriceIntent :: PriceIntent
emptyPriceIntent = PriceIntent emptyCurrency ""

emptyCurrency :: Currency
emptyCurrency = Currency "XXX" 0

emptyProduct :: Product
emptyProduct = Product "" "" emptyCurrency mempty
