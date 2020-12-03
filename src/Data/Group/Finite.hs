{-# language FlexibleInstances #-}
{-# language Safe #-}
-- |
-- Module       : Data.Group.Finite
-- Copyright    : (c) 2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>,
--                Reed Mullanix <reedmullanix@gmail.com>

-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains definitions for 'FiniteGroup'
-- along with the relevant combinators.
--
module Data.Group.Finite
( -- * Finite groups
  FiniteGroup
  -- ** Finite group combinators
, safeOrder
  -- * Finite abelian groups
, FiniteAbelianGroup
) where

import Data.Functor.Const
import Data.Functor.Identity
import Data.Group
import Data.Int
import Data.Monoid
import Data.Ord
import Data.Proxy
import Data.Word

-- $setup
--
-- >>> import qualified Prelude
-- >>> import Data.Group
-- >>> import Data.Monoid
-- >>> import Data.Semigroup
-- >>> :set -XTypeApplications

-- -------------------------------------------------------------------- --
-- Finite groups

-- | A 'FiniteGroup' is a 'Group' whose underlying set is finite.
-- This is equivalently a group object in \( FinSet \).
--
-- Finite groups often arise when considering symmetry of mathematical
-- or physical objects, when those objects admit just a finite number of
-- structure-preserving transformations. Important examples of finite groups
-- include cyclic groups and permutation groups.
--
class (Group g, Bounded g) => FiniteGroup g where

instance FiniteGroup ()
instance FiniteGroup a => FiniteGroup (Dual a)
instance FiniteGroup a => FiniteGroup (Const a b)
instance FiniteGroup a => FiniteGroup (Identity a)
instance FiniteGroup a => FiniteGroup (Proxy a)
instance (FiniteGroup a, FiniteGroup b) => FiniteGroup (a,b)
instance (FiniteGroup a, FiniteGroup b, FiniteGroup c) => FiniteGroup (a,b,c)
instance (FiniteGroup a, FiniteGroup b, FiniteGroup c, FiniteGroup d) => FiniteGroup (a,b,c,d)
instance (FiniteGroup a, FiniteGroup b, FiniteGroup c, FiniteGroup d, FiniteGroup e) => FiniteGroup (a,b,c,d,e)
instance FiniteGroup Any
instance FiniteGroup All
instance FiniteGroup (Sum Int)
instance FiniteGroup (Sum Int8)
instance FiniteGroup (Sum Int16)
instance FiniteGroup (Sum Int32)
instance FiniteGroup (Sum Int64)
instance FiniteGroup (Sum Word)
instance FiniteGroup (Sum Word8)
instance FiniteGroup (Sum Word16)
instance FiniteGroup (Sum Word32)
instance FiniteGroup (Sum Word64)
instance FiniteGroup Ordering
instance FiniteGroup a => FiniteGroup (Down a)

-- -------------------------------------------------------------------- --
-- Finite group combinators

-- | A safe version of 'order' for 'FiniteGroup's.
--
-- This is gauranteed to terminate with either @Infinite@ or @Finite@.
--
-- === __Examples__:
--
-- >>> order @(Sum Word8) 3
-- Finite 255
--
-- >>> order (Any False)
-- Finite 1
--
-- >>> order (All False)
-- Infinite
--
safeOrder :: (Eq g, FiniteGroup g) => g -> Order
safeOrder = order
{-# inline safeOrder #-}

-- -------------------------------------------------------------------- --
-- Finite abelian groups

-- | Commutative 'FiniteGroup's
--
class FiniteGroup g => FiniteAbelianGroup g

instance FiniteAbelianGroup ()
instance FiniteAbelianGroup a => FiniteAbelianGroup (Dual a)
instance FiniteAbelianGroup (Sum Int)
instance FiniteAbelianGroup (Sum Int8)
instance FiniteAbelianGroup (Sum Int16)
instance FiniteAbelianGroup (Sum Int32)
instance FiniteAbelianGroup (Sum Int64)
instance FiniteAbelianGroup (Sum Word)
instance FiniteAbelianGroup (Sum Word8)
instance FiniteAbelianGroup (Sum Word16)
instance FiniteAbelianGroup (Sum Word32)
instance FiniteAbelianGroup (Sum Word64)
instance FiniteAbelianGroup a => FiniteAbelianGroup (Const a b)
instance FiniteAbelianGroup a => FiniteAbelianGroup (Identity a)
instance FiniteAbelianGroup a => FiniteAbelianGroup (Proxy a)
instance (FiniteAbelianGroup a, FiniteAbelianGroup b) => FiniteAbelianGroup (a,b)
instance (FiniteAbelianGroup a, FiniteAbelianGroup b, FiniteAbelianGroup c) => FiniteAbelianGroup (a,b,c)
instance (FiniteAbelianGroup a, FiniteAbelianGroup b, FiniteAbelianGroup c, FiniteAbelianGroup d) => FiniteAbelianGroup (a,b,c,d)
instance (FiniteAbelianGroup a, FiniteAbelianGroup b, FiniteAbelianGroup c, FiniteAbelianGroup d, FiniteAbelianGroup e) => FiniteAbelianGroup (a,b,c,d,e)
instance FiniteAbelianGroup Ordering
instance FiniteAbelianGroup a => FiniteAbelianGroup (Down a)
