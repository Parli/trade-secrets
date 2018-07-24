module Parli.InputRow.Features
( trueFeatures
) where

import           RIO
import qualified RIO.HashSet as S
import qualified RIO.Map as M

import           Parli.InputRow.Features.Ratings
import           Parli.InputRow.Makers
import qualified Parli.InputRow.Makers.Prefab as Prefab
import           Parli.InputRow.ShimMultihotToNumeric
import           Parli.InputRow.Types

import qualified Parli.Normalizer.Types.Response as N

trueFeatures :: Truth -> Input Maker Truth
trueFeatures = runReader $ shimMultihotToNumeric =<< features

features :: Truthiness
features = mempty
  -- Parli.InputRow.Features
  <> truthy questionIdentity
  <> truthy productIdentity
  -- <> truthy sourcePopulation
  <> truthy sourceAges
  -- Parli.InputRow.Features.Ratings
  <> reviews
  <> roundups


questionIdentity :: Input Maker Truth
questionIdentity = tagInput "question" . inputFrom trueQuestion
  $  Prefab.context <&< N.analysisContext
  <> Prefab.intent <&< N.analysisIntent

productIdentity :: Input Maker Truth
productIdentity = tagInput "product" . inputFrom trueProduct
  $  Prefab.context <&< N.productContext
  <> tagInput "price" Prefab.currency <&< N.productPrice

sourcePopulation :: Input Maker Truth
sourcePopulation = tagInput "source_population" . inputFrom crawlSources
  $  feature "indicator" (indicator Sources)
  <> feature "multihot" (multihot Sources) <&< S.fromList . toList
  where
    crawlSources = fmap N.crawlSourceId . M.elems . trueCrawls

sourceAges :: Input Maker Truth
sourceAges = inputFrom sourceAges'
  $  tagInput "source_ages" Prefab.stats
  <> tagInput "source_log_ages" Prefab.stats <&< fmap (log . (+1))

sourceAges' :: Truth -> [Double]
sourceAges' truth = epochToWeeks . epochAge . N.crawlEarliest <$> legitCrawls
  where
    epochToWeeks x = fromIntegral x / (60 * 60 * 24 * 7)
    epochAge x = trueEpoch truth - x
    legitCrawls = filter isLegit . M.elems . trueCrawls $ truth
    isLegit crawl = N.crawlEarliest crawl <= trueEpoch truth
