{-# language Safe #-}
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
, toFreeGroup
) where


import Data.Group
import Data.Group.Free
import Data.Group.Free.Church

-- | The class of data structures that can be groupoidally folded.
--
class GroupFoldable t where
  -- | Apply a 'Group' fold to some container.
  --
  -- Analagous to 'foldMap' for 'Group's, this function takes a means
  -- of translating elements of some container into elements of a group,
  -- constructing the group.
  --
  goldMap :: (Group g) => (a -> g) -> t a -> g
  goldMap f t = (runFG $ toFG t) f
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

gold :: (GroupFoldable t, Group g) => t g -> g
gold = goldMap id
{-# inline gold #-}

toFreeGroup :: (GroupFoldable t, Group g) => t g -> FreeGroup g
toFreeGroup = reifyFG . toFG
{-# inline toFreeGroup #-}
