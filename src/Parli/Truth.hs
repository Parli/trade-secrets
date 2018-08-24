module Parli.Truth
( ProtoTruth(..), dogma
, Truth(..), fakeNews, lexicon
, distill
) where

import           RIO hiding (product)
import qualified RIO.Map as M

import Data.Monoid

import qualified Parli.Normalizer.Jux.Types as Type
import qualified Parli.Normalizer.Jux.Data as Data
import qualified Parli.Normalizer.Types as N
import Parli.Normalizer.Jux
import Parli.Jux

data ProtoTruth = ProtoTruth
  { protoEpoch        :: Int
  , protoVocabularies :: N.VocabularyList
  , protoQuestion     :: N.Analysis
  , protoPrice        :: Maybe N.Money
  , protoProductId    :: JuxId
  , protoEntities     :: JuxStore
  } deriving (Show)

dogma :: N.VocabularyList -> ProtoTruth
dogma vocabularies =
  ProtoTruth 0 vocabularies N.emptyAnalysis Nothing "" mempty

data Truth = Truth
  { trueEpoch        :: Int
  , trueVocabularies :: N.VocabularyList
  , trueQuestion     :: N.Analysis
  , truePrice        :: N.Money
  , trueRatings      :: [N.Rating]
  , trueCrawls       :: Map Text (Epoch, N.Crawl)
  } deriving (Show)

fakeNews :: Truth
fakeNews = Truth 0 mempty N.emptyAnalysis N.emptyMoney mempty mempty

lexicon :: N.VocabularyList -> Truth
lexicon vocabularies = fakeNews{ trueVocabularies = vocabularies }

distill :: ProtoTruth -> Truth
distill proto = fromMaybe (lexicon vocab) $ do
  productKey <- juxLookupKey product jux
  productPriceKey <- flip toAttributeKey productKey
    <$> case juxType productKey of
      Type.ProductFamily        -> Just Type.ProductFamilyPriceObj
      Type.ProductGeneration    -> Just Type.ProductGenerationPriceObj
      Type.ProductType          -> Just Type.ProductTypePriceObj
      Type.ProductConfiguration -> Just Type.ProductConfigurationPriceObj
      _                         -> Nothing
  let
    unwrapPrice (Data.ProductFamilyPriceObj        x) = Just x
    unwrapPrice (Data.ProductGenerationPriceObj    x) = Just x
    unwrapPrice (Data.ProductTypePriceObj          x) = Just x
    unwrapPrice (Data.ProductConfigurationPriceObj x) = Just x
    unwrapPrice _                                     = Nothing
    dataPrice = unwrapPrice =<< juxLookupAttribute productPriceKey jux
  price <- getAlt . fold $ Alt <$> [protoPrice, dataPrice, Just N.emptyMoney]
  pure $ Truth epoch vocab question price ratings crawls
  where
    (ProtoTruth epoch vocab question protoPrice product jux) = proto
    ratings = M.elems . M.filter cromulent $ allRatings
    cromulent rating = M.member (N.ratingCrawlId rating) crawls
    crawls = M.fromList $ (N.crawlId . snd &&& id) <$> M.elems collapsed
    collapsed = M.fromListWith collapse indexedCrawls
    indexedCrawls = (N.crawlPageId &&& (crawlEarliest &&& id)) <$> relevantCrawls
    relevantCrawls = filter nonFuture $ M.elems allCrawls
    nonFuture crawl = epoch >= N.crawlAccessedAt crawl
    allCrawls = M.fromList $ (\(Data.Crawl x) -> (crawlId &&& id) x)
      <$> juxStoreDataOfType Type.Crawl (juxEntities jux)
    allRatings = M.fromList $ (\(Data.Rating x) -> (ratingId &&& id) x)
      <$> juxStoreDataOfType Type.Rating (juxEntities jux)

collapse :: (N.Epoch, N.Crawl) -> (N.Epoch, N.Crawl) -> (N.Epoch, N.Crawl)
collapse (x,a) (y,b) = (min x y, bool a b $ N.crawlAccessedAt a < N.crawlAccessedAt b)

crawlEarliest :: N.Crawl -> N.Epoch
crawlEarliest N.Crawl
  { N.crawlAccessedAt = a, N.crawlUpdatedAt = u, N.crawlPublishedAt = p }
  = fromMaybe a . getAlt . fold . fmap Alt $ [p, u]
