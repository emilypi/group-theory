{-# language FlexibleInstances #-}
{-# language Safe #-}
-- |
-- Module       : Data.Group.Multiplicative
-- Copyright    : (c) 2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>,
--                Reed Mullanix <reedmullanix@gmail.com>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains definitions for 'MultiplicativeGroup' and
-- 'MultiplicativeAbelianGroup', along with the relevant combinators.
--
module Data.Group.Multiplicative
( -- * Multiplicative Groups
  MultiplicativeGroup
  -- ** combinators
, (/)
, (*)
, (^)
, power
  -- * Multiplicative abelian groups
, MultiplicativeAbelianGroup
) where


import Data.Functor.Const
import Data.Functor.Identity
import Data.Group
import Data.Int
import Data.Proxy
import Data.Ratio
import Data.Semigroup
import Data.Word

import Numeric.Natural

import Prelude hiding ((^), (/), (*))

infixl 7 /, *
infixr 8 ^

-- $setup
--
-- >>> import qualified Prelude
-- >>> import Data.Group
-- >>> import Data.Monoid
-- >>> import Data.Semigroup
-- >>> :set -XTypeApplications

-- -------------------------------------------------------------------- --
-- Multiplicative groups

-- | An multiplicative group is a 'Group' whose operation can be thought of
-- as multiplication in some sense.
--
-- For example, the multiplicative group of rationals \( (ℚ, 1, *) \).
--
class Group g => MultiplicativeGroup g

instance MultiplicativeGroup ()
instance MultiplicativeGroup b => MultiplicativeGroup (a -> b)
instance MultiplicativeGroup a => MultiplicativeGroup (Dual a)
instance MultiplicativeGroup All
instance MultiplicativeGroup (Product (Ratio Integer))
instance MultiplicativeGroup (Product (Ratio Natural))
instance MultiplicativeGroup (Product (Ratio Int))
instance MultiplicativeGroup (Product (Ratio Int8))
instance MultiplicativeGroup (Product (Ratio Int16))
instance MultiplicativeGroup (Product (Ratio Int32))
instance MultiplicativeGroup (Product (Ratio Int64))
instance MultiplicativeGroup (Product (Ratio Word))
instance MultiplicativeGroup (Product (Ratio Word8))
instance MultiplicativeGroup (Product (Ratio Word16))
instance MultiplicativeGroup (Product (Ratio Word32))
instance MultiplicativeGroup (Product (Ratio Word64))
instance (MultiplicativeGroup a, MultiplicativeGroup b) => MultiplicativeGroup (a,b)
instance (MultiplicativeGroup a, MultiplicativeGroup b, MultiplicativeGroup c) => MultiplicativeGroup (a,b,c)
instance (MultiplicativeGroup a, MultiplicativeGroup b, MultiplicativeGroup c, MultiplicativeGroup d) => MultiplicativeGroup (a,b,c,d)
instance (MultiplicativeGroup a, MultiplicativeGroup b, MultiplicativeGroup c, MultiplicativeGroup d, MultiplicativeGroup e) => MultiplicativeGroup (a,b,c,d,e)
instance MultiplicativeGroup a => MultiplicativeGroup (Const a b)
instance MultiplicativeGroup a => MultiplicativeGroup (Identity a)
instance MultiplicativeGroup a => MultiplicativeGroup (Proxy a)

-- | Infix alias for multiplicative inverse.
--
-- === __Examples__:
--
-- >>> let x = Product (4 :: Rational)
-- >>> x / 2
-- Product {getProduct = 2 % 1}
--
(/) :: MultiplicativeGroup a => a -> a -> a
(/) = minus
{-# inline (/) #-}

-- | Infix alias for multiplicative @('<>')@.
--
-- === __Examples__:
--
-- >>> Product (2 :: Rational) * Product (3 :: Rational)
-- Product {getProduct = 6 % 1}
--
(*) :: MultiplicativeGroup g => g -> g -> g
(*) = (<>)
{-# inline (*) #-}

-- | Infix alias for 'power'.
--
-- === __Examples__:
--
-- >>> let x = Product (3 :: Rational)
-- >>> x ^ 3
-- Product {getProduct = 27 % 1}
--
(^) :: (Integral n, MultiplicativeGroup a) => a -> n -> a
(^) = power
{-# inline (^) #-}

-- | Multiply an element of a multiplicative group by itself @n@-many times.
--
-- This represents @ℕ@-indexed powers of an element @g@ of
-- a multiplicative group, i.e. iterated products of group elements.
-- This is representable by the universal property
-- \( C(x, ∏_ₙ g) ≅ C(x, g)ⁿ \).
--
-- === __Examples__:
--
-- >>> power (Product (3 :: Rational)) 3
-- Product {getProduct = 27 % 1}
--
power :: (Integral n, MultiplicativeGroup g) => g -> n -> g
power a n = gtimes n a
{-# inline power #-}

-- -------------------------------------------------------------------- --
-- Multiplicative abelian groups

-- | A multiplicative abelian group is a 'Group' whose operation can be thought of
-- as commutative multiplication in some sense. Almost all multiplicative groups
-- are abelian.
--
class (MultiplicativeGroup g, AbelianGroup g) => MultiplicativeAbelianGroup g
instance MultiplicativeAbelianGroup ()
instance MultiplicativeAbelianGroup b => MultiplicativeAbelianGroup (a -> b)
instance MultiplicativeAbelianGroup a => MultiplicativeAbelianGroup (Dual a)
instance MultiplicativeAbelianGroup All
instance MultiplicativeAbelianGroup (Product (Ratio Integer))
instance MultiplicativeAbelianGroup (Product (Ratio Natural))
instance MultiplicativeAbelianGroup (Product (Ratio Int))
instance MultiplicativeAbelianGroup (Product (Ratio Int8))
instance MultiplicativeAbelianGroup (Product (Ratio Int16))
instance MultiplicativeAbelianGroup (Product (Ratio Int32))
instance MultiplicativeAbelianGroup (Product (Ratio Int64))
instance MultiplicativeAbelianGroup (Product (Ratio Word))
instance MultiplicativeAbelianGroup (Product (Ratio Word8))
instance MultiplicativeAbelianGroup (Product (Ratio Word16))
instance MultiplicativeAbelianGroup (Product (Ratio Word32))
instance MultiplicativeAbelianGroup (Product (Ratio Word64))
instance (MultiplicativeAbelianGroup a, MultiplicativeAbelianGroup b) => MultiplicativeAbelianGroup (a,b)
instance (MultiplicativeAbelianGroup a, MultiplicativeAbelianGroup b, MultiplicativeAbelianGroup c) => MultiplicativeAbelianGroup (a,b,c)
instance (MultiplicativeAbelianGroup a, MultiplicativeAbelianGroup b, MultiplicativeAbelianGroup c, MultiplicativeAbelianGroup d) => MultiplicativeAbelianGroup (a,b,c,d)
instance (MultiplicativeAbelianGroup a, MultiplicativeAbelianGroup b, MultiplicativeAbelianGroup c, MultiplicativeAbelianGroup d, MultiplicativeAbelianGroup e) => MultiplicativeAbelianGroup (a,b,c,d,e)
instance MultiplicativeAbelianGroup a => MultiplicativeAbelianGroup (Const a b)
instance MultiplicativeAbelianGroup a => MultiplicativeAbelianGroup (Identity a)
instance MultiplicativeAbelianGroup a => MultiplicativeAbelianGroup (Proxy a)
