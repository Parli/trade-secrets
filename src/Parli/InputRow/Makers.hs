module Parli.InputRow.Makers
( feature, tagInput, (<@<), inputFrom, (<&<), forkMaker, truthy
, numeric, toggle, onehot, embedding, multihot, indicator
, makeInput, defineInput

-- Exported to support ShimAllNumeric, hide if it goes away
, makeFeature, defineFeature, joinTag
) where

import           RIO
import qualified RIO.Map as M
import qualified RIO.Text as T

import Data.Functor.Contravariant
import GHC.Records

import Parli.InputRow.Types
import Parli.InputRow.Types.Feature


feature :: Text -> Feature Maker a -> Input Maker a
feature = M.singleton

tagInput :: Text -> Input Maker a -> Input Maker a
tagInput "" = id
tagInput t  = M.mapKeys (joinTag t)

joinTag :: Text -> Text -> Text
joinTag t "" = t
joinTag t x  = T.append t $ T.append "_" x

(<@<) :: Input Maker a -> Text -> Input Maker a
(<@<) = flip tagInput
infixl 7 <@<

inputFrom :: (b -> a) -> Input Maker a -> Input Maker b
inputFrom = fmap . contramap

(<&<) :: Input Maker a -> (b -> a) -> Input Maker b
(<&<) = flip inputFrom
infixl 7 <&<

forkMaker :: Input Maker a -> [(Text, b -> a)] -> Input Maker b
forkMaker m = foldMap $ \(tag,pipe) -> m <@< tag <&< pipe

truthy :: Input Maker Truth -> Truthiness
truthy = pure


numeric :: Feature Maker Double
numeric = Numeric . Maker $ id
toggle :: Feature Maker Bool
toggle = Toggle . Maker $ id
onehot, embedding :: Vocabulary -> Feature Maker Text
onehot v = OneHot . Maker $ Categorical v . Identity
embedding v = Embedding . Maker $ Categorical v . Identity
multihot :: Vocabulary -> Feature Maker (HashSet Text)
multihot v = MultiHot . Maker $ Categorical v
indicator :: (Foldable t) => Vocabulary -> Feature Maker (t Text)
indicator v = Indicator . Maker $ Categorical v . toList


makeInput :: b -> Input Maker b -> Input Record ()
makeInput x = fmap (flip makeFeature x)

defineInput :: Input Maker b -> Input Definition Text
defineInput = fmap defineFeature


makeFeature :: Feature Maker b -> b -> Feature Record ()
makeFeature (Numeric   (Maker m)) = Numeric   . Record . m
makeFeature (Toggle    (Maker m)) = Toggle    . Record . m
makeFeature (OneHot    (Maker m)) = OneHot    . Record . m
makeFeature (Embedding (Maker m)) = Embedding . Record . m
makeFeature (MultiHot  (Maker m)) = MultiHot  . Record . m
makeFeature (Indicator (Maker m)) = Indicator . Record . m

-- TODO: avoid using Text here?
defineFeature :: Feature Maker b -> Feature Definition Text
defineFeature (Numeric _)
  = Numeric . Definition $ "Double"
defineFeature (Toggle _)
  = Toggle . Definition $ "Bool"
defineFeature (OneHot (Maker m))  -- undefined is safe here due to laziness.
  = OneHot . Definition . getApiVocabularyKey . m $ undefined
defineFeature (Embedding (Maker m))
  = Embedding . Definition . getApiVocabularyKey . m $ undefined
defineFeature (MultiHot  (Maker m))
  = MultiHot . Definition . getApiVocabularyKey . m $ undefined
defineFeature (Indicator (Maker m))
  = Indicator . Definition . getApiVocabularyKey . m $ undefined

getApiVocabularyKey :: HasField "vocabulary" a Vocabulary => a -> Text
getApiVocabularyKey = utf8BuilderToText . display . getField @"vocabulary"
