module Parli.Normalizer
( RatingsRequest(..)
, fetchProductEntities
, fetchQuestionAnalysis
, fetchVocabularies
) where

import           RIO
import qualified RIO.Map as M
import qualified RIO.Text as T

import Data.Aeson
import Network.HTTP.Types

import Parli.InputRow.Types
import Parli.Normalizer.Internal
import Parli.Normalizer.Types.Environment
import Parli.Normalizer.Types.Response

data RatingsRequest = RatingsRequest
  { before   :: Int
  , node_ids :: [Text]
  } deriving (Eq, Show, Generic, ToJSON)

fetchProductEntities :: (MonadNormalizer env m) => RatingsRequest -> m Entities
fetchProductEntities body
  = maybe emptyEntities responseEntities
    <$> post "/entities/ratings" body
-- TODO: switch to 1.9-format /batch/input_layer

fetchQuestionAnalysis :: (MonadNormalizer env m) => Text -> m Analysis
fetchQuestionAnalysis question
  = maybe emptyAnalysis responseAnalysis
      <$> get "/getAnalysis" query
  where
    query = catMaybes [ toQueryWith queryText "query"  <$> Just question ]

fetchVocabularies :: (MonadNormalizer env m) => Int -> m VocabularyList
fetchVocabularies version = M.fromList
  <$> mapConcurrently (fetchVocabulary version) [minBound..maxBound]

fetchVocabulary :: (MonadNormalizer env m) => Int -> Vocabulary -> m (Vocabulary, [Text])
fetchVocabulary version vocab
  = maybe noVocab mkVocab
    <$> get vocabRoute query
  where
    query = [ toQueryWith queryShow "version_time" version ]
    vocabRoute = "/enums/" <> utf8BuilderToText (display vocab)
    noVocab = mkVocab mempty
    mkVocab = (vocab,)

queryText :: Text -> Maybe ByteString
queryText = Just . fromString . T.unpack

queryShow :: (Show a) => a -> Maybe ByteString
queryShow = Just . fromString . show

toQueryWith :: (a -> Maybe ByteString) -> ByteString -> a -> QueryItem
toQueryWith f label x = (label, f x)
