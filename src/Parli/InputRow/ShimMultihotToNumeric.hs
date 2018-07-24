module Parli.InputRow.ShimMultihotToNumeric
( shimMultihotToNumeric
) where

import           RIO
import qualified RIO.Map as M

import Data.Functor.Contravariant

import Parli.InputRow.Makers
import Parli.InputRow.Types
import Parli.InputRow.Types.Feature

shimMultihotToNumeric :: Input Maker Truth -> Truthiness
shimMultihotToNumeric features = do
  truth <- ask
  pure . foldMap (shim truth) . M.toList $ features

shim :: Truth -> (Text, Feature Maker Truth) -> Input Maker Truth
shim truth = \case
  (k, MultiHot  m) -> fromCategorical truth k $ getMaker m
  (k, Indicator m) -> fromCategorical truth k $ getMaker m
  x                -> uncurry M.singleton x

fromCategorical :: (Foldable f)
  => Truth -> Text -> (Truth -> Categorical f)
  -> Input Maker Truth
fromCategorical truth k m
  = M.fromList $ (joinTag k &&& go) <$> v
  where
    go vix = numeric
      >$$< fromIntegral . length . filter (==vix) . const (toList ixs)
    (Categorical v' ixs) = m truth
    v = M.findWithDefault [] v' $ trueVocabularies truth
