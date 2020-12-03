{-# language CPP #-}
{-# language FlexibleInstances #-}
{-# language Safe #-}
#if MIN_VERSION_base(4,12,0)
{-# language TypeOperators #-}
#endif
-- |
-- Module       : Data.Group
-- Copyright    : (c) 2020 Reed Mullanix, Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Reed Mullanix <reedmullanix@gmail.com>,
--                Emily Pillmore <emilypi@cohomolo.gy>
--
-- Stability    : stable
-- Portability  : non-portable
--
-- This module provides definitions 'GroupFoldable',
-- along with useful combinators.
--
module Data.Group.Foldable
( -- * Group foldable
  GroupFoldable(..)
  -- ** Group foldable combinators
, gold
, goldr
, toFreeGroup
) where


import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Identity
import Data.Group
import Data.Group.Free
import Data.Group.Free.Church
import Data.Group.Permutation
import Data.Monoid

#if MIN_VERSION_base(4,12,0)
import GHC.Generics
#endif


-- $setup
--
-- >>> import qualified Prelude
-- >>> import Data.Group
-- >>> import Data.Monoid
-- >>> import Data.Semigroup
-- >>> import Data.Word
-- >>> :set -XTypeApplications
-- >>> :set -XFlexibleContexts

-- -------------------------------------------------------------------- --
-- Group foldable

-- | The class of data structures that can be groupoidally folded.
--
class GroupFoldable t where
  -- | Apply a 'Group' fold to some container.
  --
  -- This function takes a container that can be represented as a
  -- 'FreeGroup', and simplifies the container as a word in the
  -- free group, producing a final output according to some
  -- mapping of elements into the target group.
  --
  -- The name is a pun on 'Group' and 'fold'.
  --
  -- === __Examples__:
  --
  -- >>> let x = FreeGroup $ [Left (1 :: Sum Word8), Left 2, Right 2, Right 3]
  -- >>> goldMap id x
  -- Sum {getSum = 2}
  --
  -- >>> goldMap (\a -> if a < 2 then mempty else a) x
  -- Sum {getSum = 3}
  --
  goldMap :: Group g => (a -> g) -> t a -> g
  goldMap f t = runFG (toFG t) f
  {-# inline goldMap #-}

  -- | Translate a 'GroupFoldable' container into a Church-encoded
  -- free group.
  --
  -- Analagous to 'toList' for 'Foldable', if 'toList' respected the
  -- associativity of ⊥.
  --
  toFG :: t a -> FG a
  toFG t = FG $ \k -> goldMap k t
  {-# inline toFG #-}
  {-# minimal goldMap | toFG #-}

instance GroupFoldable FG where
  toFG = id

instance GroupFoldable FreeGroup where
  toFG = reflectFG

instance GroupFoldable Sum where
  goldMap f = f . getSum

instance GroupFoldable Product where
  goldMap f = f . getProduct

instance GroupFoldable Dual where
  goldMap f = f . getDual

instance GroupFoldable (Const a) where
  goldMap _ _ = mempty

instance GroupFoldable Identity where
  goldMap f = f . runIdentity

instance (GroupFoldable f, GroupFoldable g) => GroupFoldable (Compose f g) where
  goldMap f = goldMap (goldMap f) . getCompose

#if MIN_VERSION_base(4,12,0)
instance (GroupFoldable f, GroupFoldable g) => GroupFoldable (f :*: g) where
  goldMap f (a :*: b) = goldMap f a <> goldMap f b

instance (GroupFoldable f, GroupFoldable g) => GroupFoldable (f :+: g) where
  toFG (L1 l) = toFG l
  toFG (R1 r) = toFG r

instance (GroupFoldable f, GroupFoldable g) => GroupFoldable (f :.: g) where
  goldMap f = goldMap (goldMap f) . unComp1
#endif

-- -------------------------------------------------------------------- --
-- Group foldable combinators

-- | Simplify a word in 'GroupFoldable' container as a word
-- in a 'FreeGroup'.
--
-- The name is a pun on 'Group' and 'fold'.
--
-- === __Examples__:
--
-- >>> let x = FreeGroup $ [Left (1 :: Sum Word8), Left 2, Right 2, Right 3]
-- >>> gold x
-- Sum {getSum = 2}
--
gold :: (GroupFoldable t, Group g) => t g -> g
gold = goldMap id
{-# inline gold #-}

-- | Convert a 'GroupFoldable' container into a 'FreeGroup'
--
toFreeGroup :: (GroupFoldable t, Group g) => t g -> FreeGroup g
toFreeGroup = reifyFG . toFG
{-# inline toFreeGroup #-}

-- | A right group fold from a 'GroupFoldable' container to its permutation group
--
-- Analogous to 'foldr for monoidal 'Foldable's.
--
goldr
  :: GroupFoldable t
  => Group g
  => (a -> Permutation g)
  -> t a
  -> Permutation g
goldr = goldMap
{-# inline goldr #-}
