module Parli.StatsD
( MonadStatsD, wrapTimer
) where

import RIO

import Control.Lens ((.~))
import System.Clock

import qualified Network.StatsD.Datadog as StatsD

import Parli.StatsD.Types.Environment

wrapTimer :: (MonadStatsD env m) => Text -> [StatsD.Tag] -> m a -> m a
wrapTimer label tags action = do
  start_time <- liftIO readTime
  result     <- action
  end_time   <- liftIO readTime
  logMetric StatsD.Timer label tags
    (fromIntegral . toMilliSecs $ end_time - start_time :: Int)
  pure result
  where
    readTime = getTime ProcessCPUTime
    toMilliSecs x = toNanoSecs x `div` 1000000

logMetric :: (MonadStatsD env m, StatsD.ToMetricValue a) =>
  StatsD.MetricType -> Text -> [StatsD.Tag] -> a -> m ()
logMetric mtype label tagList x
  = liftIO . flip StatsD.withDogStatsD (flip StatsD.send m)
    =<< view dogStatsSettingsL
  where
    m = StatsD.metric name mtype x & StatsD.tags .~ tagList
    name = StatsD.MetricName $ "input_layer." <> label
