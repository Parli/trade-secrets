{-# OPTIONS_GHC -fno-warn-orphans #-}
module Parli.Jux.Core.Orphans where

import RIO
import Data.Aeson
import Data.Aeson.Encoding.Internal
import Data.Binary.Builder

import Data.Text.Encoding -- unneeded for decodeUtf8'

instance FromJSON ByteString where
  parseJSON = fmap encodeUtf8 . parseJSON
instance ToJSON ByteString where
  toJSON = toJSON . decodeUtf8 -- throws on failed decode!
  -- toJSON = toJSON . either (const "") id . decodeUtf8'
  toEncoding = Encoding . fromByteString
instance FromJSONKey ByteString
instance ToJSONKey ByteString