{-# language BangPatterns #-}
{-# language FlexibleInstances #-}
{-# language Safe #-}
-- |
-- Module       : Data.Group
-- Copyright    : (c) 2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains definitions for 'Group' and 'AbelianGroup',
-- along with the relevant combinators.
--
module Data.Group
( -- * Groups
  Group(..)
  -- ** Group combinators
, (><)
, conjugate
, order
, unsafeOrder
  -- * Abelian groups
, AbelianGroup
) where



import Data.Bool
import Data.Functor.Const
import Data.Functor.Identity
import Data.Int
import Data.Monoid
import Data.Ord
import Data.Proxy
import Data.Ratio
import Data.Word

import Numeric.Natural

import Prelude hiding (negate)
import qualified Prelude

-- $setup
--
-- >>> import qualified Prelude
-- >>> import Data.Group
-- >>> import Data.Monoid
-- >>> import Data.Semigroup
-- >>> :set -XTypeApplications

infixr 6 ><

-- -------------------------------------------------------------------- --
-- Groups

class Monoid a => Group a where
  invert :: a -> a
  invert a = mempty `minus` a
  {-# inline invert #-}

  minus :: a -> a -> a
  minus a b = a <> invert b
  {-# inline minus #-}
  {-# minimal invert | minus #-}

instance Group () where
  invert = id
  {-# inline invert #-}

instance Group b => Group (a -> b) where
  invert f = invert . f
  {-# inline invert #-}

instance Group a => Group (Dual a) where
  invert (Dual a) = Dual (invert a)
  {-# inline invert #-}

instance Group Any where
  invert (Any b) = Any $ bool True False b
  {-# inline invert #-}

instance Group All where
  invert (All b) = All $ bool True False b
  {-# inline invert #-}

instance Group (Sum Integer) where
  invert = Prelude.negate
  {-# inline invert #-}

instance Group (Sum Rational) where
  invert = Prelude.negate
  {-# inline invert #-}

instance Group (Sum Int) where
  invert = Prelude.negate
  {-# inline invert #-}

instance Group (Sum Int8) where
  invert = Prelude.negate
  {-# inline invert #-}

instance Group (Sum Int16) where
  invert = Prelude.negate
  {-# inline invert #-}

instance Group (Sum Int32) where
  invert = Prelude.negate
  {-# inline invert #-}

instance Group (Sum Int64) where
  invert = Prelude.negate
  {-# inline invert #-}

instance Group (Sum Word) where
  invert = Prelude.negate
  {-# inline invert #-}

instance Group (Sum Word8) where
  invert = Prelude.negate
  {-# inline invert #-}

instance Group (Sum Word16) where
  invert = Prelude.negate
  {-# inline invert #-}

instance Group (Sum Word32) where
  invert = Prelude.negate
  {-# inline invert #-}

instance Group (Sum Word64) where
  invert = Prelude.negate
  {-# inline invert #-}

instance Group (Product Rational) where
  invert = Product . Prelude.recip . getProduct
  {-# inline invert #-}

instance Group (Product (Ratio Natural)) where
  invert = Product . Prelude.recip . getProduct
  {-# inline invert #-}

instance Group (Product (Ratio Int)) where
  invert = Product . Prelude.recip . getProduct
  {-# inline invert #-}

instance Group (Product (Ratio Int8)) where
  invert = Product . Prelude.recip . getProduct
  {-# inline invert #-}

instance Group (Product (Ratio Int16)) where
  invert = Product . Prelude.recip . getProduct
  {-# inline invert #-}

instance Group (Product (Ratio Int32)) where
  invert = Product . Prelude.recip . getProduct
  {-# inline invert #-}

instance Group (Product (Ratio Int64)) where
  invert = Product . Prelude.recip . getProduct
  {-# inline invert #-}

instance Group (Product (Ratio Word)) where
  invert = Product . Prelude.recip . getProduct
  {-# inline invert #-}

instance Group (Product (Ratio Word8)) where
  invert = Product . Prelude.recip . getProduct
  {-# inline invert #-}

instance Group (Product (Ratio Word16)) where
  invert = Product . Prelude.recip . getProduct
  {-# inline invert #-}

instance Group (Product (Ratio Word32)) where
  invert = Product . Prelude.recip . getProduct
  {-# inline invert #-}

instance Group (Product (Ratio Word64)) where
  invert = Product . Prelude.recip . getProduct
  {-# inline invert #-}

instance Group a => Group (Const a b) where
  invert = Const . invert . getConst
  {-# inline invert #-}

instance Group a => Group (Identity a) where
  invert = Identity . invert . runIdentity
  {-# inline invert #-}

instance Group Ordering where
  invert LT = GT
  invert EQ = EQ
  invert GT = LT
  {-# inline invert #-}

instance (Group a, Group b) => Group (a,b) where
  invert ~(a,b) = (invert a, invert b)
  {-# inline invert #-}

instance Group a => Group (Proxy a) where
  invert _ = Proxy

instance (Group a, Group b, Group c) => Group (a,b,c) where
  invert ~(a,b,c) = (invert a, invert b, invert c)
  {-# inline invert #-}

instance (Group a, Group b, Group c, Group d) => Group (a,b,c,d) where
  invert ~(a,b,c,d) = (invert a, invert b, invert c, invert d)
  {-# inline invert #-}

instance (Group a, Group b, Group c, Group d, Group e) => Group (a,b,c,d,e) where
  invert ~(a,b,c,d,e) = (invert a, invert b, invert c, invert d, invert e)
  {-# inline invert #-}

-- -------------------------------------------------------------------- --
-- Group combinators

-- | Apply @('<>')@, commuting its arguments. When the group is abelian,
-- @a <> b@ is identically @b <> a@.
--
(><) :: Group a => a -> a -> a
a >< b = b <> a
{-# inline (><) #-}

-- | Conjugate an element of a group by another element.
-- When the group is abelian, conjugation is the identity.
--
-- === __Examples__:
--
-- >>> let x = Sum (3 :: Int)
-- >>> conjugate x x
-- Sum {getSum = 3}
--
-- >>> let x = All True
-- >>> conjugate x (All False)
-- All {getAll = False}
--
conjugate :: Group a => a -> a -> a
conjugate a b = (b <> a) `minus` b
{-# inline conjugate #-}

-- | Calculate the order of a particular element in a group.
-- Since all elements are bounded, this will be at most @maxBound a@.
--
-- === __Examples__:
--
-- >>> order @(Sum Int) 3
-- 64
--
-- >>> order (Any False)
-- 0
--
order :: (Bounded a, Eq a, Group a) => a -> Integer
order = unsafeOrder
{-# inline order #-}

-- | Calculate the order of a particular element in a group.
--
-- __Warning:__ elements may be infinite and explode on you if the order
-- of a group element is infinite, as with any non-zero 'Integer'.
--
-- === __Examples__:
--
-- >>> unsafeOrder @(Sum Int) 3
-- 64
--
-- >>> unsafeOrder (Any False)
-- 0
--
unsafeOrder :: (Eq a, Group a) => a -> Integer
unsafeOrder = go 0 where
  go !n g
    | g == mempty = n
    | otherwise = go (succ n) (g <> g)
{-# inline unsafeOrder #-}

-- -------------------------------------------------------------------- --
-- Abelian (commutative) groups

class Group a => AbelianGroup a
instance AbelianGroup ()
instance AbelianGroup b => AbelianGroup (a -> b)
instance AbelianGroup a => AbelianGroup (Dual a)
instance AbelianGroup Any
instance AbelianGroup All
instance AbelianGroup (Sum Integer)
instance AbelianGroup (Sum Rational)
instance AbelianGroup (Sum Int)
instance AbelianGroup (Sum Int8)
instance AbelianGroup (Sum Int16)
instance AbelianGroup (Sum Int32)
instance AbelianGroup (Sum Int64)
instance AbelianGroup (Sum Word)
instance AbelianGroup (Sum Word8)
instance AbelianGroup (Sum Word16)
instance AbelianGroup (Sum Word32)
instance AbelianGroup (Sum Word64)
instance AbelianGroup (Product (Ratio Integer))
instance AbelianGroup (Product (Ratio Natural))
instance AbelianGroup (Product (Ratio Int))
instance AbelianGroup (Product (Ratio Int8))
instance AbelianGroup (Product (Ratio Int16))
instance AbelianGroup (Product (Ratio Int32))
instance AbelianGroup (Product (Ratio Int64))
instance AbelianGroup (Product (Ratio Word))
instance AbelianGroup (Product (Ratio Word8))
instance AbelianGroup (Product (Ratio Word16))
instance AbelianGroup (Product (Ratio Word32))
instance AbelianGroup (Product (Ratio Word64))
instance AbelianGroup a => AbelianGroup (Const a b)
instance AbelianGroup a => AbelianGroup (Identity a)
instance AbelianGroup a => AbelianGroup (Proxy a)
instance AbelianGroup Ordering
instance (AbelianGroup a, AbelianGroup b) => AbelianGroup (a,b)
instance (AbelianGroup a, AbelianGroup b, AbelianGroup c) => AbelianGroup (a,b,c)
instance (AbelianGroup a, AbelianGroup b, AbelianGroup c, AbelianGroup d) => AbelianGroup (a,b,c,d)
instance (AbelianGroup a, AbelianGroup b, AbelianGroup c, AbelianGroup d, AbelianGroup e) => AbelianGroup (a,b,c,d,e)
