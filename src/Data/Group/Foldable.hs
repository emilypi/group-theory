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
) where


import Data.Group
import Data.Group.Free


-- | The class of data structures that can be groupoidally folded.
--
class GroupFoldable t where
    gold :: (Group g) => t g -> g
    gold = goldMap id

    goldMap :: (Group g) => (a -> g) -> t a -> g

    toFreeGroup :: t a -> FreeGroup a
    -- goldr :: (a -> Iso a a) -> b -> t a -> a
    {-# minimal goldMap | toFreeGroup #-}
