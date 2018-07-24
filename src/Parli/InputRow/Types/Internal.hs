{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Parli.InputRow.Types.Internal where

import RIO

import Data.Aeson.Types
import Data.Functor.Contravariant

newtype Maker a b = Maker { getMaker :: b -> a }
instance Contravariant (Maker a) where
  contramap f (Maker m) = Maker $ m . f

newtype Record a b = Record { getRecord :: a} -- type Record = Const
  deriving (Eq, Show, Generic)
deriving newtype instance (ToJSON a) => ToJSON (Record a b)

newtype Definition a b = Definition { getDefinition :: b }
  deriving (Eq, Show)

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
