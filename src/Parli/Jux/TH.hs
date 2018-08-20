{-# LANGUAGE TemplateHaskell #-}
module Parli.Jux.TH
( deriveJuxDataToJSON
, deriveJuxDataFromJSON
) where

import RIO

import Data.Aeson
import Language.Haskell.TH

deriveSumAsNewtype :: DecsQ -> (Con -> Q Clause) -> Name -> DecsQ
deriveSumAsNewtype mkBase mkClause a = do
  TyConI (DataD _ _ _ _ constructors _) <- reify a
  clauses <- traverse mkClause constructors
  [     InstanceD o [] (AppT t (ConT _)) [FunD f _      ]] <- mkBase
  pure [InstanceD o [] (AppT t (ConT a)) [FunD f clauses]]

data T0 = T0
deriveJuxDataToJSON :: Name -> DecsQ
deriveJuxDataToJSON = deriveSumAsNewtype mkBase mkClause where
  mkBase = [d|
    instance ToJSON T0 where
      toJSON T0 = undefined
    |]
  mkClause (NormalC name [_]) = do
    (fieldP, fieldE) <- (varP &&& varE) <$> newName "x"
    clause
      [conP name [fieldP]]
      (normalB [e|toJSON $(fieldE)|])
      []
  mkClause _ = error
    "Can't use deriveJuxDataToJSON with multi-parameter constructors"

deriveJuxDataFromJSON :: Name -> DecsQ
deriveJuxDataFromJSON = deriveSumAsNewtype mkBase mkClause where
  mkBase = [d|
    instance FromJSON T0 where
      parseJSON T0 = undefined
    |]
  mkClause (NormalC name [_]) = do
    (fieldP, fieldE) <- (varP &&& varE) <$> newName "x"
    clause
      [fieldP]
      (normalB [e|$(conE name) <$> parseJSON $(fieldE)|])
      []
  mkClause _ = error
    "Can't use deriveJuxDataFromJSON with multi-parameter constructors"
