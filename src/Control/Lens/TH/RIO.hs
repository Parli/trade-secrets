module Control.Lens.TH.RIO where

import           RIO
import qualified RIO.Char as C

import Control.Lens ((.~))
import Control.Lens.TH
import Language.Haskell.TH

makeRioLenses, makeRioClassy, makeRioClassOnly :: Name -> DecsQ
makeRioLenses = makeLensesWith rioLensRules
makeRioClassy = makeLensesWith rioClassyRules
makeRioClassOnly = makeLensesWith rioClassOnlyRules

rioLensRules, rioClassyRules, rioClassOnlyRules :: LensRules
rioLensRules = lensRules
  & lensField .~ rioFieldNamer
rioClassyRules = classyRules
  & lensField .~ rioFieldNamer
  & lensClass .~ rioClassyNamer
rioClassOnlyRules = classyRules
  & lensField .~ (\_ _ _ -> [])
  & lensClass .~ rioClassyNamer

rioFieldNamer :: FieldNamer
rioFieldNamer _ _ name = case nameBase name of
  x:xs -> [TopName (mkName $ C.toLower x:xs<>"L")]
  _    -> []

rioClassyNamer :: ClassyNamer
rioClassyNamer name = case nameBase name of
  n@(x:xs) -> Just (mkName $ "Has"<>n, mkName $ C.toLower x:xs<>"L")
  []       -> Nothing
