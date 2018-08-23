module Parli.Normalizer
( RatingsRequest(..)
, fetchRatingsEntities
, fetchQuestionAnalysis
, fetchVocabularies
) where

import           RIO
import qualified RIO.Map as M
import qualified RIO.Text as T

import Data.Aeson
import Network.HTTP.Types

import Parli.Normalizer.Internal
import Parli.Normalizer.Jux
import Parli.Normalizer.Types

data RatingsRequest = RatingsRequest
  { before   :: Int
  , node_ids :: [Text]
  } deriving (Eq, Show, Generic, ToJSON)

fetchRatingsEntities :: (MonadNormalizer env m) => RatingsRequest -> m JuxStore
fetchRatingsEntities body
  = maybe mempty id <$> post "/batch/input_layer" body

fetchQuestionAnalysis :: (MonadNormalizer env m) => Text -> m Analysis
fetchQuestionAnalysis question
  = maybe emptyAnalysis id <$> get "/getAnalysis" query
  where
    query = catMaybes [toQueryWith queryText "query" <$> Just question]

fetchVocabularies :: (MonadNormalizer env m) => Int -> m VocabularyList
fetchVocabularies version
  = M.fromList <$> mapConcurrently go [minBound..maxBound]
  where
    go v = (v,) . maybe mempty id <$> get (route v) query
    route = mappend "/enums/" . utf8BuilderToText . display
    query = pure $ toQueryWith queryShow "version_time" version

queryText :: Text -> Maybe ByteString
queryText = Just . fromString . T.unpack

queryShow :: (Show a) => a -> Maybe ByteString
queryShow = Just . fromString . show

toQueryWith :: (a -> Maybe ByteString) -> ByteString -> a -> QueryItem
toQueryWith f label x = (label, f x)
