{-# language BangPatterns #-}
{-# language FlexibleInstances #-}
{-# language PatternSynonyms #-}
{-# language Safe #-}
{-# language TypeOperators #-}
{-# language ViewPatterns #-}
-- |
-- Module       : Data.Group
-- Copyright    : (c) 2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>,
--                Reed Mullanix <reedmullanix@gmail.com>
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
  -- ** Group order
, Order(..)
, pattern Infinity
, pattern Finitary
  -- * Abelian groups
, AbelianGroup
  -- * Group endomorphisms
, GroupEndo(..)
) where


import Data.Bool
import Data.Functor.Const
import Data.Functor.Identity
import Data.Semigroup (stimes)
import Data.Int
import Data.Monoid
import Data.Ord
import Data.Proxy
import Data.Ratio
import Data.Word

import Numeric.Natural

import GHC.Generics

import Prelude hiding (negate, exponent)
import qualified Prelude

-- $setup
--
-- >>> import qualified Prelude
-- >>> import Data.Group
-- >>> import Data.Monoid
-- >>> import Data.Semigroup
-- >>> :set -XTypeApplications
-- >>> :set -XFlexibleContexts

infixr 6 ><

-- -------------------------------------------------------------------- --
-- Groups

class Monoid a => Group a where
  invert :: a -> a
  invert a = mempty `minus` a
  {-# inline invert #-}

  -- | Similar to 'stimes' from 'Data.Semigroup', but handles
  -- negative numbers by using 'invert'.
  --
  -- === __Examples:__
  --
  -- >>> gtimes 2 (Sum 3)
  -- Sum {getSum = 6}
  -- >>> gtimes (-3) (Sum 3)
  -- Sum {getSum = -9}
  --
  gtimes :: (Integral n) => n -> a -> a
  gtimes n a
    | n == 0 = mempty
    | n > 0 = stimes n a
    | otherwise = stimes (abs n) (invert a)
  {-# inline gtimes #-}

  -- | 'Group' subtraction.
  --
  -- This function denotes principled 'Group' subtraction, where
  -- @a `minus` b@ translates into @a <> (invert b)@. This is because
  -- subtraction as an operator is non-associative, but the operation
  -- described in terms of addition and inversion is.
  --
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

instance (Group (f a), Group (g a)) => Group ((f :*: g) a) where
  invert (f :*: g) = invert f :*: invert g

instance Group (f (g a)) => Group ((f :.: g) a) where
  invert (Comp1 fg) = invert (Comp1 fg)

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
-- Note: the first argument is the conjugating element so that
-- it can be fixed to produce conjugacy classes of second argument.
--
-- === __Examples__:
--
-- >>> let x = Sum (3 :: Int)
-- >>> conjugate x x
-- Sum {getSum = 3}
--
-- >>> let x = All True
-- >>> conjugate (All False) x
-- All {getAll = False}
--
conjugate :: Group a => a -> a -> a
conjugate g a = (g <> a) `minus` g
{-# inline conjugate #-}

-- -------------------------------------------------------------------- --
-- Group order

-- | The order of a group element.
--
-- The order of a group element can either be infinite,
-- as in the case of @All False@, or finite, as in the
-- case of @All True@.
--
data Order = Infinite | Finite !Natural
  deriving (Eq, Show)

-- | Unidirectional pattern synonym for the infinite order of a
-- group element.
--
pattern Infinity :: (Eq g, Group g) => () => g
pattern Infinity <- (order -> Infinite)

-- | Unidirectional pattern synonym for the finite order of a
-- group element.
--
pattern Finitary :: (Eq g, Group g) => () => Natural -> g
pattern Finitary n <- (order -> Finite n)

-- | Calculate the exponent of a particular element in a group.
--
-- __Warning:__ If 'order' expects a 'FiniteGroup', this is gauranteed
-- to terminate. However, this is not true of groups in general. This will
-- spin forever if you give it something like non-zero @Sum Integer@.
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
order :: (Eq g, Group g) => g -> Order
order a = go 0 a where
  go !n g
    -- guard against ().
    | g == mempty, n > 0 = Finite n
    -- guard against infinite cases like @All False@.
    | g == a, n > 0 = Infinite
    | otherwise = go (succ n) (g <> a)
{-# inline order #-}

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

instance (AbelianGroup (f a), AbelianGroup (g a)) => AbelianGroup ((f :*: g) a)
instance AbelianGroup (f (g a)) => AbelianGroup ((f :.: g) a)

-- -------------------------------------------------------------------- --
-- Group endomorphisms

-- | A group homomorphism from the group to itself.
--
-- 'GroupEndo' forms a near-ring for any group, since it is not necessarily
-- additive. When @g@ is an 'AbelianGroup', 'GroupEndo' forms a ring.
--
newtype GroupEndo g = GroupEndo
  { appGroupEndo :: g -> g }

instance Semigroup (GroupEndo g) where
  GroupEndo g <> GroupEndo g' = GroupEndo (g' . g)

instance Monoid (GroupEndo g) where
  mempty = GroupEndo id

instance Group g => Group (GroupEndo g) where
  invert = GroupEndo . (invert .) . appGroupEndo

instance AbelianGroup g => AbelianGroup (GroupEndo g)
