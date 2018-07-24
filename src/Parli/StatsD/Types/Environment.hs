module Parli.StatsD.Types.Environment where

import RIO

import qualified Network.StatsD.Datadog as StatsD

newtype StatsDEnv = StatsDEnv { getStatsDEnv :: StatsD.DogStatsSettings }
class HasDogStatsSettings env where
  dogStatsSettingsL :: Lens' env StatsD.DogStatsSettings
instance HasDogStatsSettings StatsDEnv where
  dogStatsSettingsL = lens getStatsDEnv (const StatsDEnv)
instance Show StatsDEnv where
  show (StatsDEnv (StatsD.DogStatsSettings host port))
    = "StatsD {" <> host <> ":" <> show port <> "}"

type MonadStatsD env m =
  ( MonadIO m, MonadReader env m, HasDogStatsSettings env )
