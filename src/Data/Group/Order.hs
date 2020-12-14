{-# language Safe #-}
{-# language FlexibleInstances #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
-- |
-- Module       : Data.Group.Order
-- Copyright    : (c) 2020 Emily Pillmore
--                Koji Miyazato <viercc@gmail.com>
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>,
--                Reed Mullanix <reedmullanix@gmail.com>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains definitions for 'GroupOrder'.
module Data.Group.Order
( -- * Group order
  GroupOrder(..)
  -- ** Order
, Order(..)
, pattern Infinitary
, pattern Finitary
, orderForBits
, lcmOrder
, FiniteGroup
, finiteOrder
) where


import Data.Bits
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Group
import Data.Group.Finite (FiniteGroup, finiteOrder)
import Data.Int
import Data.Monoid
import Data.Ord (Down(..))
import Data.Proxy (Proxy)
import Data.Word


import Numeric.Natural (Natural)


-- -------------------------------------------------------------------- --
-- Group order

-- | The order of a group element.
--
-- The order of a group element can either be infinite,
-- as in the case of @Sum Integer@, or finite, as in the
-- case of @Sum Word8@.
--
data Order = Infinite | Finite !Natural
  deriving (Eq, Show)

-- | Unidirectional pattern synonym for the infinite order of a
-- group element.
--
pattern Infinitary :: (GroupOrder g) => g
pattern Infinitary <- (order -> Infinite)

-- | Unidirectional pattern synonym for the finite order of a
-- group element.
--
pattern Finitary :: (GroupOrder g) => Natural -> g
pattern Finitary n <- (order -> Finite n)

-- | @lcmOrder x y@ calculates the least common multiple of two 'Order's.
--
--   If both @x@ and @y@ are finite, it returns @'Finite' r@ where @r@
--   is the least common multiple of them. Otherwise, it returns 'Infinite'.
--
-- === __Examples__:
--
-- >>> lcmOrder (Finite 2) (Finite 5)
-- Finite 10
-- >>> lcmOrder (Finite 2) (Finite 10)
-- Finite 10
-- >>> lcmOrder (Finite 1) Infinite
-- Infinite
--
lcmOrder :: Order -> Order -> Order
lcmOrder (Finite m) (Finite n) = Finite (lcm m n)
lcmOrder _          _          = Infinite

-- | The typeclass of groups, equipped with the function
-- computing the order of a specific element of a group.
--
-- The order of @x@ is the smallest positive integer @k@
-- such that @'Data.Group.gtimes' k x == 'mempty'@. If there are no such
-- integers, the order of @x@ is defined to be infinity.
--
-- /Note:/ For any valid instances of 'GroupOrder',
-- @order x == Finite 1@ holds if and only if @x == mempty@.
--
-- === __Examples__:
--
-- >>> order (3 :: Sum Word8)
-- Finite 256
-- >>> order (16 :: Sum Word8)
-- Finite 16
-- >>> order (0 :: Sum Integer)
-- Finite 1
-- >>> order (1 :: Sum Integer)
-- Infinite
--
class (Eq g, Group g) => GroupOrder g where
    -- | The order of an element of a group.
    --
    -- @order x@ must be @Finite k@ if the order of @x@ is
    -- finite @k@, and must be @Infinite@ otherwise.
    --
    -- For a type which is also 'FiniteGroup',
    -- @'Finite' . 'finiteOrder'@ is a valid implementation of 'order',
    -- if not efficient.
    order :: g -> Order

instance GroupOrder () where
    order _ = Finite 1

instance GroupOrder (Proxy a) where
    order _ = Finite 1

instance GroupOrder (Sum Integer) where
    order 0 = Finite 1
    order _ = Infinite

instance GroupOrder (Sum Rational) where
    order 0 = Finite 1
    order _ = Infinite

instance GroupOrder (Sum Int) where order = orderForBits
instance GroupOrder (Sum Int8) where order = orderForBits
instance GroupOrder (Sum Int16) where order = orderForBits
instance GroupOrder (Sum Int32) where order = orderForBits
instance GroupOrder (Sum Int64) where order = orderForBits
instance GroupOrder (Sum Word) where order = orderForBits
instance GroupOrder (Sum Word8) where order = orderForBits
instance GroupOrder (Sum Word16) where order = orderForBits
instance GroupOrder (Sum Word32) where order = orderForBits
instance GroupOrder (Sum Word64) where order = orderForBits

instance (Eq g, GroupOrder g) => GroupOrder (Abelianizer g) where
  order Quot = Finite 1
  order (Commuted g) = order g

-- | Given a number @x :: a@ represented by fixed-width binary integers,
-- return the minimum positive integer @2^n@ such that
-- @(fromInteger (2^n) * x :: a) == 0@.
--
zeroFactor :: FiniteBits a => a -> Natural
zeroFactor a = bit (finiteBitSize a - countTrailingZeros a)

-- | An efficient implementation of 'order' for additive group of
--   fixed-width integers, like 'Int' or 'Word8'.
--
orderForBits :: (Integral a, FiniteBits a) => Sum a -> Order
orderForBits (Sum a) = Finite (zeroFactor a)

instance GroupOrder (Product Rational) where
    order 1 = Finite 1
    order _ = Infinite

instance (GroupOrder a, GroupOrder b) => GroupOrder (a,b) where
    order (a,b) = order a `lcmOrder` order b

instance (GroupOrder a, GroupOrder b, GroupOrder c) => GroupOrder (a,b,c) where
    order (a,b,c) = order ((a,b),c)

instance (GroupOrder a, GroupOrder b, GroupOrder c, GroupOrder d)
        => GroupOrder (a,b,c,d) where
    order (a,b,c,d) = order ((a,b),(c,d))
instance (GroupOrder a, GroupOrder b, GroupOrder c, GroupOrder d, GroupOrder e)
        => GroupOrder (a,b,c,d,e) where
    order (a,b,c,d,e) = order ((a,b,c),(d,e))

{- Safe Haskell doesn't allow GND, at least for now.
{-# language
  GeneralizedNewtypeDeriving,
  StandaloneDeriving,
  DerivingStrategies
#-}
deriving newtype instance GroupOrder a => GroupOrder (Down a)
deriving newtype instance GroupOrder a => GroupOrder (Dual a)
deriving newtype instance GroupOrder a => GroupOrder (Const a b)
deriving newtype instance GroupOrder a => GroupOrder (Identity a)
-}
instance GroupOrder a => GroupOrder (Down a) where
    order (Down a) = order a

instance GroupOrder a => GroupOrder (Dual a) where
    order = order . getDual

instance GroupOrder a => GroupOrder (Const a b) where
    order = order . getConst

instance GroupOrder a => GroupOrder (Identity a) where
    order = order . runIdentity
