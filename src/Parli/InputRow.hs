module Parli.InputRow
( Input, Record, Definition, VocabularyList
, ProtoTruth(..)
, defineFeatures
, makeFeatures
) where

import           RIO hiding (product)
import qualified RIO.Map as M

import           Parli.InputRow.Features
import           Parli.InputRow.Makers
import           Parli.InputRow.Types
import qualified Parli.Normalizer.Types.Response as N

defineFeatures :: VocabularyList -> Input Definition Text
defineFeatures = defineInput . trueFeatures . lexicon

makeFeatures :: ProtoTruth -> Input Record ()
makeFeatures = (makeInput <*> trueFeatures) . distill

distill :: ProtoTruth -> Truth
distill proto = fromMaybe (lexicon vocab) $ do
  p <- listToMaybe . M.elems $ allProducts
  let product = p{N.productPrice = price}
  pure $ Truth epoch vocab question product ratings crawls
  where
    crawls = M.fromList $ (N.crawlId &&& id) <$> M.elems collapsed
    collapsed = M.fromListWith collapse
      $ (N.crawlPageId &&& id) <$> M.elems allCrawls
    ratings = M.elems . M.filter okay $ allRatings
    okay rating = M.member (N.ratingCrawlId rating) crawls
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

data ProtoTruth = ProtoTruth
  { protoEpoch        :: Int
  , protoVocabularies :: VocabularyList
  , protoQuestion     :: N.Analysis
  , protoEntities     :: N.Entities
  , protoPrice        :: N.Currency
  } deriving (Show)

lexicon :: VocabularyList -> Truth
lexicon vocabularies = fakeNews{ trueVocabularies = vocabularies }
