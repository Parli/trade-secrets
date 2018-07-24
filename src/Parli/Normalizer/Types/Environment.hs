{-# LANGUAGE TemplateHaskell #-}
module Parli.Normalizer.Types.Environment where

import Magicbane.HTTPClient
import RIO

import Control.Concurrent.QSem
import Control.Lens.TH
import Control.Lens.TH.RIO

data NormalizerConnection = NormalizerConnection
  { _normalizerRequestTimeout :: !Double
  , _normalizerServer         :: !Text
  , _normalizerUsername       :: !Text
  , _normalizerPassword       :: !Text
  } deriving (Generic)
makeLenses ''NormalizerConnection

data NormalizerAuthToken = NormalizerAuthToken
  { _normalizerToken          :: !Text
  , _normalizerTokenLifespan  :: !Int
  , _normalizerTokenTimestamp :: !Int
  } deriving (Generic)
makeLenses ''NormalizerAuthToken

data NormalizerEnv = NormalizerEnv
  { _normalizerConnection :: !NormalizerConnection
  , _normalizerAuthToken  :: !(IORef NormalizerAuthToken)
  , _normalizerOutgoingQ  :: !QSem
  }
makeLenses ''NormalizerEnv
makeRioClassOnly ''NormalizerEnv

type MonadNormalizer env m =
  ( MonadCatch m, MonadIO m, MonadUnliftIO m, MonadReader env m
  , HasLogFunc env, HasHttpManager env, HasNormalizerEnv env
  )
