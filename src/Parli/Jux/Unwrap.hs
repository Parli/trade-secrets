module Parli.Jux.Unwrap where

import RIO
import qualified RIO.Set as S
import Parli.Jux.Core.Types
import Data.Monoid (Alt(..))

data JuxUnwrap' a b c where
  JuxUnwrap' :: JuxLabel a =>
    { juxUnwrapType :: a
    , juxUnwrapper  :: (b -> c)
    } -> JuxUnwrap' a b c
instance Eq (JuxUnwrap' a b c) where JuxUnwrap' t _ == JuxUnwrap' u _ = t == u
instance Ord (JuxUnwrap' a b c) where JuxUnwrap' t _ <= JuxUnwrap' u _ = t <= u

newtype JuxUnwrap a b c = JuxUnwrap { getJuxUnwrap :: Set (JuxUnwrap' a b c) }
  deriving newtype (Monoid, Semigroup)
type JuxUnwrapEntity' e a = JuxUnwrap e (JuxEntityData e) a
type JuxUnwrapAttribute' e a = JuxUnwrap (JuxAttributeType e) (JuxAttributeData e) a
type JuxUnwrapQuery' q a = JuxUnwrap q (JuxQueryRequest q) a
type JuxUnwrapResponse' q a = JuxUnwrap q (JuxQueryResponse q) a

mkJuxUnwrap :: JuxLabel a => a -> (b -> c) -> JuxUnwrap a b c
mkJuxUnwrap t un = JuxUnwrap . S.singleton $ JuxUnwrap' t un

juxUnwraps :: JuxLabel a => JuxUnwrap a b c -> [JuxUnwrap' a b c]
juxUnwraps = S.toList . getJuxUnwrap

juxUnwrapTypes :: JuxLabel a => JuxUnwrap a b c -> [a]
juxUnwrapTypes = fmap juxUnwrapType . juxUnwraps

juxDataChecking :: JuxLabel a => (b -> a) -> JuxUnwrap a b c -> b -> Maybe c
juxDataChecking is un d
  = getAlt . fold . fmap (Alt . tryUnwrap) $ juxUnwraps un
  where
  tryUnwrap (JuxUnwrap' t un') = if is d == t then Just (un' d) else Nothing

juxDataKeyed :: JuxLabel a => JuxUnwrap a b c -> JuxKey a -> b -> Maybe c
juxDataKeyed un (JuxKey t _) = juxDataChecking (const t) un
