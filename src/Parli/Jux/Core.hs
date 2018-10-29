module Parli.Jux.Core
( module Parli.Jux.Core
, module Parli.Jux.Internal
, module Parli.Jux.Core.TH
, module Parli.Jux.Core.Types
) where

import RIO

import Data.Aeson.Types

import Parli.Jux.Internal
import Parli.Jux.Core.TH
import Parli.Jux.Core.Types

juxToJSONKey :: (JuxLabel a) => ToJSONKeyFunction a
juxToJSONKey = toJSONKeyText showJuxLabel

juxFromJSONKey :: (JuxLabel a) => Text -> FromJSONKeyFunction a
juxFromJSONKey target
  = FromJSONKeyText $ either error id . readJuxLabel (juxReadError target)