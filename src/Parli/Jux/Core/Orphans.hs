{-# OPTIONS_GHC -fno-warn-orphans #-}
module Parli.Jux.Core.Orphans where

import           RIO
import qualified RIO.HashMap as HM
import qualified RIO.HashSet as HS

import Data.Serialize
import Data.Serialize.Text ()

deriving instance Serialize a => Serialize (Identity a)

instance (Eq k, Hashable k, Serialize k, Serialize v)
  => Serialize (HashMap k v) where
  get = HM.fromList <$> Data.Serialize.get
  put = put . HM.toList

instance (Eq a, Hashable a, Serialize a) => Serialize (HashSet a) where
  get = HS.fromList <$> Data.Serialize.get
  put = put . HS.toList