module Parli.Normalizer.Types.Values.Aeson
( deriveNormalizerObjectJSON
, normalizerEnumToJSON
, normalizerEnumParseJSON
) where

import           RIO
import qualified RIO.Partial as Unsafe

import Data.Aeson.TH
import Data.Aeson.Types
import Language.Haskell.TH
import Parli.Jux

deriveNormalizerObjectJSON :: Name -> DecsQ
deriveNormalizerObjectJSON name = deriveJSON defaultOptions
  { fieldLabelModifier
    = let
      parts = id &&& drop (length $ namePrefix name)
      go y x
        | x == "id" = y
        -- | x `elem` ["id", "type"] = y
        | otherwise = x
    in uncurry go . parts . juxLabelToWire
  } name

normalizerEnumToJSON :: Show a => Name -> a -> Value
normalizerEnumToJSON name x = case words $ show x of
  []  -> error "Received empty string from Show instance"
  [c] -> String . fromString . juxLabelToWire $ drop (length $ nameBase name) c
  _:l -> String . Unsafe.read $ unwords l

normalizerEnumParseJSON :: Read a => (Text -> a) -> Name -> Value -> Parser a
normalizerEnumParseJSON unknown name = withText (show name) $ \v ->
  either pure pure $ readJuxLabel unknown (fromString (namePrefix name) <> v)

namePrefix :: Name -> String
namePrefix name = juxLabelToWire (nameBase name) <> "_"
