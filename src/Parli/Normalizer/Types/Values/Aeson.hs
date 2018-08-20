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
  { constructorTagModifier
    = let
      parts = id &&& dropNamePrefix name
      go y x
        | x `elem` ["id", "type"] = y
        | otherwise = x
    in uncurry go . parts . juxLabelToWire
  } name

normalizerEnumToJSON :: Show a => Name -> a -> Value
normalizerEnumToJSON name x = case words $ show x of
  []            -> error "Received empty string from Show instance"
  [constructor] -> String . fromString $ dropNamePrefix name constructor
  _ : label     -> String . Unsafe.read . unwords $ label

normalizerEnumParseJSON :: Read a => (Text -> a) -> Name -> Value -> Parser a
normalizerEnumParseJSON unknown name = withText (show name) $ \v ->
  either pure pure $ readJuxLabel unknown (fromString (namePrefix name) <> v)

dropNamePrefix :: Show a => a -> String -> String
dropNamePrefix name = drop $ 1 + length (namePrefix name)

namePrefix :: Show a => a -> String
namePrefix name = showJuxLabel name <> "_"
