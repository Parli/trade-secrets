module Parli.InputRow.Makers.Prefab where

import           RIO
import qualified RIO.Vector as V
import qualified RIO.Vector.Partial as V'

import           Data.Foldable.Safe
import qualified Statistics.Sample as Stats

import           Parli.InputRow.Makers
import           Parli.InputRow.Types
import qualified Parli.Normalizer.Types.Response as N


logInt :: Integral a => Input Maker a
logInt = feature "log" numeric <&< log . (+1) . fromIntegral

currency :: Input Maker N.Currency
currency = tagInput "amount" logInt <&< N.currencyAmount

count :: Foldable t => Input Maker (t a)
count = tagInput "count" logInt <&< length

context :: Input Maker N.Context
context
  =  contextSet "categories" (multihot Categories) <&< N.contextCategories
  <> contextSet "keywords" (multihot Keywords) <&< N.contextKeywords
  <> contextSet "filters" (multihot Filters) <&< N.contextFilters
  where
    contextSet t m = tagInput t
      $  feature "" m
      <> count

intent :: Input Maker N.PriceIntent
intent = tagInput "price_intent"
  $  feature "" (onehot PriceIntents) <&< N.intentType
  <> currency <&< N.intentAmount

stats :: Input Maker [Double]
stats = inputFrom (V.fromList @Vector @Double)
  $  count
  <> feature "nonempty" toggle <&< not . null
  <> feature "min" numeric <&< defaulting 0 V'.minimum
  <> feature "max" numeric <&< defaulting 0 V'.maximum
  <> feature "arithmetic_mean" numeric <&< ifNan 0 . Stats.mean
  <> feature "harmonic_mean" numeric <&< ifNan 0 . Stats.harmonicMean
  <> feature "geometric_mean" numeric <&< ifNan 0 . Stats.geometricMean
  <> feature "skewness" numeric <&< ifNan 0 . Stats.skewness
  <> feature "kurtosis" numeric <&< ifNan 0 . Stats.kurtosis
  <> feature "variance" numeric <&< Stats.variance
  <> feature "standard_deviation" numeric <&< Stats.stdDev
  <> feature "mean_standard_error" numeric <&< ifNan 0 . Stats.stdErrMean

ifNan :: Double -> Double -> Double
ifNan d x
  | x /= x || abs x == 1/0 = d
  | otherwise              = x
