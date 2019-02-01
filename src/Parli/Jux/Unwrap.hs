module Parli.Jux.Unwrap where

import RIO
import qualified RIO.Map as M
import Parli.Jux.Core.Types
import Data.Monoid (Alt(..))

newtype JuxUnwrap a b c = JuxUnwrap { getJuxUnwrap :: Map a (b -> c) }
  deriving newtype (Monoid, Semigroup)
instance Functor (JuxUnwrap a b) where
  fmap f u = JuxUnwrap $ (f .) <$> getJuxUnwrap u
type JuxUnwrapEntity' e a = JuxUnwrap e (JuxEntityData e) a
type JuxUnwrapAttribute' e a = JuxUnwrap (JuxAttributeType e) (JuxAttributeData e) a
type JuxUnwrapQuery' q a = JuxUnwrap q (JuxQueryRequest q) a
type JuxUnwrapResponse' q a = JuxUnwrap q (JuxQueryResponse q) a

mkJuxUnwrap :: JuxLabel a => a -> (b -> c) -> JuxUnwrap a b c
mkJuxUnwrap t = JuxUnwrap . M.singleton t

juxUnwrapPairs :: JuxLabel a => JuxUnwrap a b c -> [(a, b -> c)]
juxUnwrapPairs = M.toList . getJuxUnwrap

juxUnwrappers :: JuxLabel a => JuxUnwrap a b c -> [b -> c]
juxUnwrappers = M.elems . getJuxUnwrap

juxUnwrapTypes :: JuxLabel a => JuxUnwrap a b c -> [a]
juxUnwrapTypes = M.keys . getJuxUnwrap

juxDataChecking :: JuxLabel a => (b -> a) -> JuxUnwrap a b c -> b -> Maybe c
juxDataChecking is un d
  = getAlt . fold . fmap (Alt . tryUnwrap) $ juxUnwrapPairs un
  where
  tryUnwrap (t, un') = if is d == t then Just (un' d) else Nothing

juxDataKeyed :: JuxLabel a => JuxUnwrap a b c -> JuxKey a -> b -> Maybe c
juxDataKeyed un (JuxKey t _) = juxDataChecking (const t) un
