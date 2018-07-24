module Parli.InputRow.Journalism where

import           RIO
import qualified RIO.HashSet as S
import qualified RIO.Map as M

import           Parli.InputRow.Types
import qualified Parli.Normalizer.Types.Response as N

whenQuestionHasKeywords :: Truth -> Truth
whenQuestionHasKeywords
  truth@Truth{ trueRatings = ratings, trueQuestion = question}
  = truth { trueRatings = go }
  where
    go| S.null source = []
      | otherwise     = ratings
    source = N.contextKeywords $ N.analysisContext question

ratingsOfType :: N.RatingType -> Truth -> Truth
ratingsOfType t = filterRatings (const $ (== t) . N.ratingType)

ratingsOfCrawlType :: N.CrawlType -> Truth -> Truth
ratingsOfCrawlType t = filterRatings okay
  where
    okay Truth{ trueCrawls = crawls } N.Rating{ N.ratingCrawlId = ix }
      = maybe False crawlOkay $ M.lookup ix crawls
    crawlOkay p = N.crawlType p == t

filterRatings :: (Truth -> N.Rating -> Bool) -> Truth -> Truth
filterRatings via truth@Truth{ trueRatings = ratings }
  = truth{ trueRatings = filter (via truth) ratings }

checkKeywordsOverlap :: SetOverlap -> Truth -> N.Rating -> Bool
checkKeywordsOverlap threshold = checkKeywords okay
  where
    okay source target = setOverlap source target >= threshold

checkKeywordsSubset :: Truth -> N.Rating -> Bool
checkKeywordsSubset = checkKeywords subsetOf

checkKeywords :: (HashSet Text -> HashSet Text -> Bool)
  -> Truth -> N.Rating -> Bool
checkKeywords predicate Truth{ trueQuestion = question } rating
  = predicate source target
  where
    source = N.contextKeywords $ N.analysisContext question
    target = N.contextKeywords $ N.ratingContext rating

subsetOf :: (Hashable a, Eq a) => HashSet a -> HashSet a -> Bool
subsetOf source target = S.intersection source target == target

data SetOverlap = None | Weak | Strong | Total
  deriving (Eq, Ord, Bounded, Enum, Show)

setOverlap :: (Hashable a, Eq a) => HashSet a -> HashSet a -> SetOverlap
setOverlap source target
  | coverage == 1  = Total
  | coverage > 0.5 = Strong
  | coverage > 0   = Weak
  | otherwise      = None
  where
    coverage
      | total == 0 = 1.0 :: Double
      | otherwise  = fromIntegral overlap / fromIntegral total
    overlap = length $ S.intersection source target
    total   = length source
