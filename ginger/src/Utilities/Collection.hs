module Utilities.Collection where

import Data.List qualified as L
import Data.Map qualified as M
import Data.Set qualified as S

-- | An alias for the map type constructor to increase legibility of type definitions.
-- > a ↦ b ≡ Map a b
type a ↦ b = M.Map a b

-- | Short-hand for map insertion.
(⇒) :: Ord k => M.Map k a -> (k, a) -> M.Map k a
m ⇒ (k, v) = M.insert k v m

-- | Nested map insertion. Insert element at outer map-inner map key pair.
(⊔) :: (Ord k1, Ord k2) => M.Map k1 (M.Map k2 a) -> (k1, k2, a) -> M.Map k1 (M.Map k2 a)
m ⊔ (k1, k2, v) = m ⇒ (k1, maybe (M.singleton k2 v) (M.insert k2 v) (M.lookup k1 m))

-- | Nested map insertion. Insert map of inner elements at outer map key.
(⨆) :: (Foldable t, Ord k1, Ord k2) => M.Map k1 (M.Map k2 v) -> (k1, t (k2, v)) -> M.Map k1 (M.Map k2 v)
m ⨆ (k, kvs) = L.foldl (\m' (k', v) -> m' ⊔ (k, k', v)) m kvs

-- | Nested map insertion. Merge outer maps.
(⩏) :: (Foldable t, Ord k1, Ord k2) => M.Map k1 (M.Map k2 v) -> t (k1, [(k2, v)]) -> M.Map k1 (M.Map k2 v)
m ⩏ kkvs = L.foldl (⨆) m kkvs

-- | Short-hand for multi-element map insertion.
(⭆) :: Ord k => M.Map k a -> [(k, a)] -> M.Map k a
m ⭆ kvs = M.union (M.fromList kvs) m

-- | Collection can be implemented by types which can be aggregated.
class Collection a where
  zero :: a
  union :: a -> a -> a
  intersect :: a -> a -> a

-- | Short-hand for empty collection.
(∅) :: Collection a => a
(∅) = zero

-- | Short-hand for collection union.
(∪) :: Collection a => a -> a -> a
(∪) = union

-- | Short-hand for collection intersection.
(∩) :: Collection a => a -> a -> a
(∩) = intersect

-- | Maps implement collection by point-wise union.
instance (Ord a, Collection b) => Collection (M.Map a b) where
  zero = M.empty
  union = M.unionWith (∪)
  intersect = M.intersectionWith (∩)

-- | Sets implement collection by point-wise union.
instance Ord a => Collection (S.Set a) where
  zero = S.empty
  union = S.union
  intersect = S.intersection

-- | Lists implement collection as concatenation for union, or shortest prefix for intersection.
instance Eq a => Collection [a] where
  zero = []
  union = (++)
  intersect a1 a2 =
    case (a1, a2) of
      ([], _) -> []
      (_, []) -> []
      (a : a1', a' : a2') -> if a == a' then a : (a1' ∩ a2') else []
