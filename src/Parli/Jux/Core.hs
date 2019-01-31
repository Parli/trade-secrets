module Parli.Jux.Core
( module Parli.Jux.Core
, module Parli.Jux.Internal
, module Parli.Jux.Core.TH
, module Parli.Jux.Core.Types
, JuxUnwrap, mkJuxUnwrap
) where

import RIO

import Data.Aeson.Types

import Parli.Jux.Internal
import Parli.Jux.Core.TH
import Parli.Jux.Core.Types
import Parli.Jux.Unwrap

juxToJSONKey :: (JuxLabel a) => ToJSONKeyFunction a
juxToJSONKey = toJSONKeyText showJuxLabel

juxFromJSONKey :: (JuxLabel a) => Text -> FromJSONKeyFunction a
juxFromJSONKey target
  = FromJSONKeyText $ either error id . readJuxLabel (juxReadError target)

wrapParseJSON :: FromJSON a => (a -> v) -> Value -> Parser v
wrapParseJSON w = fmap w . parseJSON
