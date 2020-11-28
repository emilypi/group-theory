{-# language BangPatterns #-}
{-# language FlexibleInstances #-}
{-# language Safe #-}
-- |
-- Module       : Data.Group.Cyclic
-- Copyright    : (c) 2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains definitions for 'CyclicGroup'
-- along with the relevant combinators.
--
module Data.Group.Cyclic
( -- * Cyclic groups
  CyclicGroup(..)
  -- ** Combinators
, generate
) where

import Data.Functor.Const
import Data.Functor.Identity
import Data.Group
import Data.Int
import Data.List
import Data.Monoid
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
-- Cyclic groups

-- | A 'CyclicGroup' is a 'Group' that is generated by a single element.
-- This element is called a /generator/ of the group. There can be many
-- generators for a group, e.g., any representative of an equivalence
-- class of prime numbers of the integers modulo @n@, but to make things
-- easy, we ask for only one generator.
--
class Group g => CyclicGroup g where
  generator :: g
  {-# minimal generator #-}

instance CyclicGroup () where
  generator = ()
  {-# inline generator #-}

instance CyclicGroup b => CyclicGroup (a -> b) where
  generator = const generator
  {-# inlinable generator #-}

instance CyclicGroup a => CyclicGroup (Dual a) where
  generator = Dual (invert generator)
  {-# inlinable generator #-}

instance CyclicGroup (Sum Integer) where
  generator = 0
  {-# inline generator #-}

instance CyclicGroup (Sum Rational) where
  generator = 0
  {-# inline generator #-}

instance CyclicGroup (Sum Int) where
  generator = 0
  {-# inline generator #-}

instance CyclicGroup (Sum Int8) where
  generator = 0
  {-# inline generator #-}

instance CyclicGroup (Sum Int16) where
  generator = 0
  {-# inline generator #-}

instance CyclicGroup (Sum Int32) where
  generator = 0
  {-# inline generator #-}

instance CyclicGroup (Sum Int64) where
  generator = 0
  {-# inline generator #-}

instance CyclicGroup (Sum Word) where
  generator = 0
  {-# inline generator #-}

instance CyclicGroup (Sum Word8) where
  generator = 0
  {-# inline generator #-}

instance CyclicGroup (Sum Word16) where
  generator = 0
  {-# inline generator #-}

instance CyclicGroup (Sum Word32) where
  generator = 0
  {-# inline generator #-}

instance CyclicGroup (Sum Word64) where
  generator = 0
  {-# inline generator #-}

instance CyclicGroup a => CyclicGroup (Const a b) where
  generator = Const generator
  {-# inlinable generator #-}

instance CyclicGroup a => CyclicGroup (Identity a) where
  generator = Identity generator
  {-# inlinable generator #-}

instance CyclicGroup a => CyclicGroup (Proxy a) where
  generator = Proxy
  {-# inlinable generator #-}

instance (CyclicGroup a, CyclicGroup b) => CyclicGroup (a,b) where
  generator = (generator, generator)
  {-# inlinable generator #-}

instance (CyclicGroup a, CyclicGroup b, CyclicGroup c) => CyclicGroup (a,b,c) where
  generator = (generator, generator, generator)
  {-# inlinable generator #-}

instance (CyclicGroup a, CyclicGroup b, CyclicGroup c, CyclicGroup d) => CyclicGroup (a,b,c,d)  where
  generator = (generator, generator, generator, generator)
  {-# inlinable generator #-}

instance (CyclicGroup a, CyclicGroup b, CyclicGroup c, CyclicGroup d, CyclicGroup e) => CyclicGroup (a,b,c,d,e) where
  generator = (generator, generator, generator, generator, generator)
  {-# inlinable generator #-}

-- -------------------------------------------------------------------- --
-- Cyclic group combinators

-- | Lazily generate all elements of a 'CyclicGroup' from its generator.
--
-- /Note/: the use of 'unfoldr' allows this to fuse.
--
generate :: (Eq a, CyclicGroup a) => [a]
generate = unfoldr go (generator, 0 :: Integer)
  where
    go (a, !n)
      | a == mempty, n > 0 = Nothing
      | otherwise = Just (a, (a <> generator, succ n))

-- | Classify elements of a 'CyclicGroup'.
--
-- Apply a classifying function @a -> Bool@ to the elements
-- of a 'CyclicGroup' as generated by its designated generator.
--
classify :: (Eq a, CyclicGroup a) => (a -> Bool) -> [a]
classify p = filter p generate
