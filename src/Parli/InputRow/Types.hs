module Parli.InputRow.Types
( module Parli.InputRow.Types
, module Parli.InputRow.Types.Internal
) where

import RIO

import           Parli.InputRow.Types.Feature
import           Parli.InputRow.Types.Internal
import qualified Parli.Normalizer.Types.Response as N

data Truth = Truth
  { trueEpoch        :: Int
  , trueVocabularies :: VocabularyList
  , trueQuestion     :: N.Analysis
  , trueProduct      :: N.Product
  , trueRatings      :: [N.Rating]
  , trueCrawls       :: Map Text N.Crawl
  } deriving (Show)

fakeNews :: Truth
fakeNews = Truth 0 mempty N.emptyAnalysis N.emptyProduct mempty mempty

type VocabularyList = Map Vocabulary [Text]

type Input f b = Map Text (Feature f b)

type Truthiness = Reader Truth (Input Maker Truth)
instance Semigroup Truthiness where
  x' <> y' = do
    x <- x'
    y <- y'
    pure $ x <> y
instance Monoid Truthiness where
  mempty = pure mempty
  mappend = (<>)
