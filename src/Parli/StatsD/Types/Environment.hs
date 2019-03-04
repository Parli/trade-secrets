{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Parli.StatsD.Types.Environment where

import RIO
import Control.Lens.TH.RIO

import qualified Network.StatsD.Datadog as StatsD

newtype StatsDEnv = StatsDEnv { getStatsDEnv :: StatsD.DogStatsSettings }
instance Show StatsDEnv where
  show (StatsDEnv (StatsD.DogStatsSettings host port))
    = "StatsD {" <> host <> ":" <> show port <> "}"
makeRioClassOnly ''StatsDEnv
class HasDogStatsSettings env where
  dogStatsSettingsL :: Lens' env StatsD.DogStatsSettings
instance HasStatsDEnv env => HasDogStatsSettings env where
  dogStatsSettingsL = statsDEnvL . lens getStatsDEnv (const StatsDEnv)

type MonadStatsD env m =
  ( MonadIO m, MonadReader env m, HasDogStatsSettings env )
