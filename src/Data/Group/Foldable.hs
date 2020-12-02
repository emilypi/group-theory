{-# language FlexibleInstances #-}
{-# language Safe #-}
{-# language TypeOperators #-}
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
) where


import Data.Bifunctor
import Data.Functor.Const
import Data.Functor.Identity
import Data.Group
import Data.Group.Free
import Data.Group.Free.Church
import Data.Monoid

import GHC.Generics


-- | The class of data structures that can be groupoidally folded.
--
class GroupFoldable t where
  -- | Apply a 'Group' fold to some container.
  --
  -- Analagous to 'foldMap' for 'Group's, this function takes a means
  -- of translating elements of some container into elements of a group,
  -- constructing the group.
  --
  -- The name is a pun on 'Group' and 'fold'.
  --
  goldMap :: (Group g) => (a -> g) -> t a -> g
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

  -- | Execute a group fold on a 'GroupFoldable' container.
  --
  -- The name is a pun on 'Group' and 'fold'.
  --
  gold :: Group g => t g -> g
  gold = goldMap id
  {-# inline gold #-}

  -- | Convert a 'GroupFoldable' container into a 'FreeGroup'
  --
  toFreeGroup :: Group g => t g -> FreeGroup g
  toFreeGroup = reifyFG . toFG
  {-# inline toFreeGroup #-}

  -- | A right group fold from a 'GroupFoldable' container to its permutation group
  --
  -- Analogous to 'foldr for monoidal 'Foldable's.
  --
  goldr
    :: Group g
    => (a -> (g -> g, g -> g))
    -> t a
    -> (g -> g, g -> g)
  goldr f = bimap appGroupEndo appGroupEndo
    . goldMap (bimap GroupEndo GroupEndo . f)


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

instance (GroupFoldable f, GroupFoldable g) => GroupFoldable (f :*: g) where
  goldMap f (a :*: b) = goldMap f a <> goldMap f b

instance (GroupFoldable f, GroupFoldable g) => GroupFoldable (f :+: g) where
  toFG (L1 l) = toFG l
  toFG (R1 r) = toFG r

instance (GroupFoldable f, GroupFoldable g) => GroupFoldable (f :.: g) where
  goldMap f = goldMap (goldMap f) . unComp1
