module Parli.Jux.Internal where

import           RIO
import qualified RIO.Text as T
import           Text.Casing
import           Text.Read

juxLabelToWire, juxWireToLabel :: String -> String
juxLabelToWire = toQuietSnake . fromHumps
juxWireToLabel = toPascal . fromSnake

showJuxLabel :: (Show a, IsString s) => a -> s
showJuxLabel = fromString . juxLabelToWire . show

readJuxLabel :: (Read a) => (Text -> e) -> Text -> Either e a
readJuxLabel toError t = case reads s of
  [(a,[])] -> Right a
  _        -> Left (toError t)
  where s = juxWireToLabel . T.unpack $ t

juxReadError :: Text -> Text -> String
juxReadError target source
  = "Could not read type "<> show target <>" from string "<> show source
