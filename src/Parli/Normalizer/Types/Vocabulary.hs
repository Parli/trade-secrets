module Parli.Normalizer.Types.Vocabulary where

import RIO

import Data.Aeson
import Data.Aeson.Types

data Vocabulary
  = Categories | Keywords | Filters | Sources | PriceIntents | Labels
  deriving (Eq, Ord, Show, Bounded, Enum, Generic, Hashable)
instance Display Vocabulary where
  display = \case
    Categories   -> "categories"
    Keywords     -> "keywords"
    Filters      -> "filters"
    Sources      -> "sources"
    PriceIntents -> "price_intents"
    Labels       -> "labels"
instance ToJSON Vocabulary where
  toJSON = String . utf8BuilderToText . display
instance ToJSONKey Vocabulary where
  toJSONKey = toJSONKeyText $ utf8BuilderToText . display
instance FromJSON Vocabulary where
  parseJSON = withText "rows_req" $ \v -> pure $ case v of  -- TODO: fix partial pattern
    "categories"    -> Categories
    "keywords"      -> Keywords
    "filters"       -> Filters
    "sources"       -> Sources
    "price_intents" -> PriceIntents
    "labels"        -> Labels

type VocabularyList = Map Vocabulary [Text]
