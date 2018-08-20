-- {-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE StrictData #-}
-- {-# LANGUAGE TypeFamilyDependencies #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Parli.Jux.Response where

-- import           RIO
-- import qualified RIO.Map as M
-- import qualified RIO.Map.Unchecked as M'

-- import Data.Aeson

-- import Parli.Jux.Base

-- class
--   ( Eq a, Ord a
--   , ToJSONKey a
--   , ToJSON (Entity a), ToJSON (Attribute a)
--   ) => ResponseData a where
--   type Entity a    = x | x -> a
--   type Attribute a = y | y -> a
--   getEntityObject :: Entity a -> a
--   getAttrObject   :: Attribute a -> a
--   getAttrName     :: Attribute a -> Text
--   hasObjectData   :: Entity a -> Bool
--   -- hasAttrData     :: Attribute a -> Bool

-- newtype EntitySet' id a = EntitySet {getEntitySet :: Map id (Entity a) }
--   deriving (Generic)
-- deriving instance (Eq id, Eq a, Eq (Entity a)) => Eq (EntitySet' id a)
-- deriving instance (Show id, Show a, Show (Entity a)) => Show (EntitySet' id a)

-- instance (IdDynamic id, ResponseData a, Ord a, Show a, ToJSON (Entity a))
--   => ToJSON (EntitySet' id a) where
--   toJSON (EntitySet m) = toJSON . partitionMapBy getEntityObject $ m

-- instance (IdDynamic id) => Semigroup (EntitySet' id a) where
--   EntitySet ma <> EntitySet mb = EntitySet (M.union ma mb)

-- instance (IdDynamic id) => Monoid (EntitySet' id a) where
--   mempty = EntitySet M.empty
--   mappend = (<>)


-- newtype AttributeSet' id a = AttributeSet { getAttributeSet :: [(id, Attribute a)] }
--   deriving (Generic)
-- deriving instance (IdDynamic id,   Eq a,   Eq (Attribute a)) =>   Eq (AttributeSet' id a)
-- deriving instance (IdDynamic id, Show a, Show (Attribute a)) => Show (AttributeSet' id a)

-- instance
--   ( ResponseData a
--   , Ord a
--   , ToJSONKey a
--   , ToJSON (Attribute a)
--   , IdDynamic id
--   ) => ToJSON (AttributeSet' id a) where
--   toJSON (AttributeSet m) = toJSON . formatContents . fmap innerMap . topLvlMap $ m where
--     formatContents = weightLift M'.fromAscList
--     topLvlMap :: [(id, Attribute a)] -> Map a    [(id, Attribute a)]
--     innerMap  :: [(id, Attribute a)] -> Map Text [(id, Attribute a)]
--     topLvlMap = partitionBy (getAttrObject . snd)
--     innerMap  = partitionBy (getAttrName   . snd)

-- instance (Ord a) => Semigroup (AttributeSet' id a) where
--   AttributeSet ma <> AttributeSet mb = AttributeSet (ma <> mb)

-- instance (Ord a) => Monoid (AttributeSet' id a) where
--   mempty = AttributeSet mempty
--   mappend = (<>)


-- data Attributes' id a = Attributes
--   { entities :: AttributeSet' id a
--   } deriving (Generic)
-- deriving instance (IdDynamic id, Eq a, Eq (AttributeSet' id a)) => Eq (Attributes' id a)
-- deriving instance (IdDynamic id, Show a, Show (AttributeSet' id a)) => Show (Attributes' id a)
-- deriving instance (IdDynamic id, Show a, ToJSON a, ToJSONKey a, ToJSON (AttributeSet' id a))
--   => ToJSON (Attributes' id a)

-- data APIResult' id a = APIResult
--   { entities   :: EntitySet' id a
--   , attributes :: AttributeSet' id a
--   } deriving (Generic)
-- deriving instance (Eq a, Eq (EntitySet' id a), Eq (AttributeSet' id a)) => Eq (APIResult' id a)
-- deriving instance (Show a, Show (EntitySet' id a), Show (AttributeSet' id a)) => Show (APIResult' id a)
-- deriving instance (Show a, ToJSON a, ToJSONKey a, ToJSON (EntitySet' id a), ToJSON (AttributeSet' id a))
--   => ToJSON (APIResult' id a)


-- weightLift :: (Functor g, Functor f) => (a -> b) -> f (g a) -> f (g b)
-- weightLift = fmap . fmap

-- partitionBy :: (Ord k) => (a -> k) -> [a] -> Map k [a]
-- partitionBy key = M.fromListWith (<>) . fmap (key &&& pure)

-- partitionMapBy :: (Eq i, Ord k) => (a -> k) -> Map i a -> Map k (Map i a)
-- partitionMapBy key = fmap M'.fromAscList . partitionBy (key . snd) . M'.toAscList
