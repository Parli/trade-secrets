module Parli.InputRow.Features.Ratings
( reviews
, roundups
) where

import           RIO
import qualified RIO.HashSet as S
import qualified RIO.Map as M

import           Parli.InputRow.Journalism
import           Parli.InputRow.Makers
import qualified Parli.InputRow.Makers.Prefab as Prefab
import           Parli.InputRow.Types
import qualified Parli.Normalizer.Types.Response as N


reviews :: Truthiness
reviews
  = tagInput "reviews" . inputFrom (ratingsOfCrawlType N.CrawlTypeReview)
    <$> ratings

roundups :: Truthiness
roundups
  = tagInput "roundups" . inputFrom (ratingsOfCrawlType N.CrawlTypeRoundup)
    <$> ratings


ratings :: Truthiness
ratings = --do
  -- ratingsPerKeyword' <- ratingsPerKeyword
  pure
    $  ratingsGlobal
    <> ratingsByKeywordCoverage
    -- <> ratingsPerKeyword'


ratingsGlobal :: Input Maker Truth
ratingsGlobal = ratingsTypeGroupings <@< "global"

ratingsByKeywordCoverage :: Input Maker Truth
ratingsByKeywordCoverage = inputFrom whenQuestionHasKeywords
  $  ratingsKeywordsEmpty
  <> ratingsKeywordsOverlap <@< "overlap"
  <> ratingsKeywordsOverlap <@< "subset"
     <&< filterRatings checkKeywordsSubset

ratingsKeywordsEmpty :: Input Maker Truth
ratingsKeywordsEmpty = ratingsTypeGroupings <@< "empty_keywords"
  <&< filterRatings (const $ S.null . N.contextKeywords . N.ratingContext)

ratingsKeywordsOverlap :: Input Maker Truth
ratingsKeywordsOverlap = tagInput "keywords"
  $ forkMaker ratingsTypeGroupings
  [ ("weak", filterRatings $ checkKeywordsOverlap Weak)
  , ("strong", filterRatings $ checkKeywordsOverlap Strong)
  , ("total", filterRatings $ checkKeywordsOverlap Total)
  ]

ratingsPerKeyword :: Truthiness
ratingsPerKeyword = do
  vs <- asks trueVocabularies
  truthy $ perKeyword vs <@< "with_keyword"
  where
    perKeyword vs = forkMaker ratingsTypeGroupings $ do
      let v = maybe [] id $ M.lookup Keywords vs
      (id &&& filterRatings . hasKeyword) <$> v
    hasKeyword ix = checkKeywords $ const (S.member ix)

ratingsTypeGroupings :: Input Maker Truth
ratingsTypeGroupings
  =  unifiedRatingsMaker
  <> discreteRatingsMaker


unifiedRatingsMaker :: Input Maker Truth
unifiedRatingsMaker = inputFrom reduction
  $  Prefab.stats <@< "unified_ratings" <&< catMaybes
  <> Prefab.count <@< "unified_mentions"
  where
    reduction Truth{ trueRatings = rs }
      = M.elems . M.fromListWith max . fmap unifiedRatingValue $ rs

unifiedRatingValue :: N.Rating -> (Text, Maybe Double)
unifiedRatingValue
  N.Rating{N.ratingType = t, N.ratingValue = v, N.ratingCrawlId = ix}
  = (ix, join $ go t <$> v)
  where
    go N.RatingTypeRank     = unifyRankScore
    go N.RatingTypeScore    = unifyRankScore
    go N.RatingTypeTopPick  = bullshit 1
    go N.RatingTypeRunnerUp = bullshit 0.75
    go _                    = const Nothing
    unifyRankScore (N.RatingValueRank (N.Rank position _))
      = Just $ exp 2 / (log (2/3 + fromIntegral position) + exp 2)
    unifyRankScore (N.RatingValueScore (N.Score value limit))
      = Just $ value / limit
    bullshit = const . Just . sigmoid . exp
    sigmoid p = 1 / (1 + exp (-p))


discreteRatingsMaker :: Input Maker Truth
discreteRatingsMaker
  =  forkMaker discreteRatingsStatsMaker
     [ ("rank_ratings", ratingsOfType N.RatingTypeRank)
     , ("on_list_ratings", ratingsOfType N.RatingTypeOnList)
     , ("score_ratings", ratingsOfType N.RatingTypeScore)
     , ("subscore_ratings", ratingsOfType N.RatingTypeSubscore)
     , ("attribute_score_ratings", ratingsOfType N.RatingTypeAttributeScore)
     ]
  <> forkMaker discreteRatingsCountMaker
     [ ("top_pick_ratings", ratingsOfType N.RatingTypeTopPick)
     , ("runner_up_ratings", ratingsOfType N.RatingTypeRunnerUp)
     , ("award_ratings", ratingsOfType N.RatingTypeAward)
     , ("considered_ratings", ratingsOfType N.RatingTypeConsidered)
     , ("mentioned_ratings", ratingsOfType N.RatingTypeMentioned)
     ]

discreteRatingsStatsMaker :: Input Maker Truth
discreteRatingsStatsMaker = Prefab.stats
  <&< M.elems . M.fromListWith max . indexedNormalizedRatingValues
  where
    indexedNormalizedRatingValues
      = fmap (fmap normalizeRatingValue) . indexedRatingValues
    indexedRatingValues
      = catMaybes . fmap indexedRatingValue . trueRatings
    indexedRatingValue = sequence . (N.ratingCrawlId &&& N.ratingValue)

discreteRatingsCountMaker :: Input Maker Truth
discreteRatingsCountMaker = Prefab.count
  <&< S.fromList . fmap N.ratingCrawlId . trueRatings

normalizeRatingValue :: N.RatingValue -> Double
normalizeRatingValue (N.RatingValueRank (N.Rank rank _))
  = 1 / (1 + log (fromIntegral rank))
normalizeRatingValue (N.RatingValueScore (N.Score score limit))
 = score / limit
