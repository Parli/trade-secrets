module Parli.Truth
( ProtoTruth(..), dogma
, Truth(..), fakeNews, lexicon
, distill
) where

import           RIO hiding (product)
import qualified RIO.Map as M

import qualified Parli.Normalizer.Types as N

data ProtoTruth = ProtoTruth
  { protoEpoch        :: Int
  , protoVocabularies :: N.VocabularyList
  , protoQuestion     :: N.Analysis
  , protoEntities     :: N.Entities
  , protoPrice        :: N.Currency
  } deriving (Show)

dogma :: N.VocabularyList -> ProtoTruth
dogma vocabularies =
  ProtoTruth 0 vocabularies N.emptyAnalysis N.emptyEntities N.emptyCurrency

data Truth = Truth
  { trueEpoch        :: Int
  , trueVocabularies :: N.VocabularyList
  , trueQuestion     :: N.Analysis
  , trueProduct      :: N.Product
  , trueRatings      :: [N.Rating]
  , trueCrawls       :: Map Text N.Crawl
  } deriving (Show)

fakeNews :: Truth
fakeNews = Truth 0 mempty N.emptyAnalysis N.emptyProduct mempty mempty

lexicon :: N.VocabularyList -> Truth
lexicon vocabularies = fakeNews{ trueVocabularies = vocabularies }

distill :: ProtoTruth -> Truth
distill proto = fromMaybe (lexicon vocab) $ do
  p <- listToMaybe . M.elems $ allProducts
  let product = p{N.productPrice = price}
  pure $ Truth epoch vocab question product ratings crawls
  where
    ratings = M.elems . M.filter cromulent $ allRatings
    cromulent rating = M.member (N.ratingCrawlId rating) crawls
    crawls = M.fromList $ (N.crawlId &&& id) <$> M.elems collapsed
    collapsed = M.fromListWith collapse indexedCrawls
    indexedCrawls = (N.crawlPageId &&& id) <$> relevantCrawls
    relevantCrawls = filter nonFuture $ M.elems allCrawls
    nonFuture crawl = epoch >= N.crawlAccessed crawl
    (ProtoTruth epoch vocab question entities price) = proto
    (N.Entities allProducts allRatings allCrawls) = entities

collapse :: N.Crawl -> N.Crawl -> N.Crawl
collapse
  x@ N.Crawl { N.crawlAccessed = xA, N.crawlEarliest = xE }
  y@ N.Crawl { N.crawlAccessed = yA, N.crawlEarliest = yE }
  = latest { N.crawlEarliest = crawlEarliest }
  where
    latest = if xA > yA then x else y
    crawlEarliest = min xE yE
