module Parli.KeyText
( KeyText -- Constructor not exported; guarantees Utf8 validation
, stringKey, textKey
, keyBytes, keyString, keyText
, isNullKey
) where

import RIO

import qualified RIO.Text as T
import qualified Data.ByteString.Short as SB

import Codec.Serialise
import Data.Aeson.Encoding.Internal
import Data.Aeson.Types
import Data.Binary.Builder
import Data.Text.Encoding

newtype KeyText = KeyText ShortByteString
  deriving stock (Read, Show, Data, Typeable, Generic)
  deriving newtype (Eq, Ord, Semigroup, Monoid, NFData, Serialise, Hashable)
instance IsString KeyText where
  fromString = stringKey
instance Display KeyText where
  display = displayBytesUtf8 . keyBytes
instance FromJSON KeyText where
  parseJSON = fmap textKey . parseJSON
instance ToJSON KeyText where
  toJSON = toJSON . keyText
  toEncoding = Encoding . fromByteString . keyBytes
instance FromJSONKey KeyText where
  fromJSONKey = FromJSONKeyText textKey
instance ToJSONKey KeyText where
  toJSONKey = toJSONKeyText keyText

-- Construct
stringKey :: String -> KeyText
stringKey = textKey . T.pack

textKey :: Text -> KeyText
textKey = KeyText . toShort . encodeUtf8

-- Deconstruct
keyBytes :: KeyText -> ByteString
keyBytes (KeyText x) = fromShort x

keyString :: KeyText -> String
keyString = T.unpack . keyText

keyText :: KeyText -> Text
keyText = decodeUtf8 . keyBytes

-- Inspect
isNullKey :: KeyText -> Bool
isNullKey (KeyText x) = SB.null x
