{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
module Parli.InputRow.Types.Feature where

import           RIO
import qualified RIO.Text as T

import Data.Csv (ToField(..))

import Data.Aeson.Types
import Data.Functor.Contravariant

import Parli.InputRow.Types.Internal

data Feature f b
  = Numeric   (f Double b)
  | Toggle    (f Bool b)
  | OneHot    (f (Categorical Identity) b)
  | Embedding (f (Categorical Identity) b)
  | MultiHot  (f (Categorical HashSet) b)
  | Indicator (f (Categorical []) b)
  deriving (Generic)

instance Contravariant (Feature Maker) where
  contramap f (Numeric m)   = Numeric   $ contramap f m
  contramap f (Toggle m)    = Toggle    $ contramap f m
  contramap f (OneHot m)    = OneHot    $ contramap f m
  contramap f (Embedding m) = Embedding $ contramap f m
  contramap f (MultiHot m)  = MultiHot  $ contramap f m
  contramap f (Indicator m) = Indicator $ contramap f m

deriving instance Eq (Feature Record ())
deriving instance Show (Feature Record ())
deriving instance ToJSON (Feature Record ())
instance ToField (Feature Record ()) where
  toField (Numeric   (Record x)) = toField x
  toField (Toggle    (Record x)) = toField $ show x
  toField (OneHot    (Record x)) = toField x
  toField (Embedding (Record x)) = toField x
  toField (MultiHot  (Record x)) = toField x
  toField (Indicator (Record x)) = toField x

deriving instance Eq (Feature Definition Text)
deriving instance Show (Feature Definition Text)
instance ToJSON (Feature Definition Text) where
  toJSON (Numeric _) = object
    [ "tag" .= ("Numeric"::Text) ]
  toJSON (Toggle _) = object
    [ "tag" .= ("Toggle"::Text) ]
  toJSON (OneHot (Definition v)) = object
    [ "tag" .= ("OneHot"::Text)
    , "vocabulary" .= v
    ]
  toJSON (Embedding (Definition v)) = object
    [ "tag" .= ("Embedding"::Text)
    , "vocabulary" .= v
    ]
  toJSON (MultiHot (Definition v)) = object
    [ "tag" .= ("MultiHot"::Text)
    , "vocabulary" .= v
    ]
  toJSON (Indicator (Definition v)) = object
    [ "tag" .= ("Indicator"::Text)
    , "vocabulary" .= v
    ]

data Categorical f = Categorical
  { vocabulary :: Vocabulary
  , verbiage   :: f Text
  } deriving (Generic)

deriving instance (Eq (f Text)) => Eq (Categorical f)
deriving instance (Show (f Text)) => Show (Categorical f)
instance (ToJSON (f Text)) => ToJSON (Categorical f) where
  toJSON = toJSON . verbiage

instance ToField (Categorical Identity) where
  toField = T.encodeUtf8 . runIdentity . verbiage
instance ToField (Categorical []) where
  toField = textListToField . verbiage
instance ToField (Categorical HashSet) where
  toField = textListToField . toList . verbiage
textListToField :: [Text] -> ByteString
textListToField ts = T.encodeUtf8 $ "[" <> ts' <> "]"
  where
    ts' = T.intercalate "," $ enquote <$> ts
    enquote t = T.append "\"" $ T.append t "\""
