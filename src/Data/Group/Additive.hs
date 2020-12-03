{-# language FlexibleInstances #-}
-- {-# language Safe #-}
-- |
-- Module       : Data.Group.Additive
-- Copyright    : (c) 2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>,
--                Reed Mullanix <reedmullanix@gmail.com>
--
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains definitions for 'AdditiveGroup' and 'AdditiveAbelianGroup',
-- along with the relevant combinators.
--
module Data.Group.Additive
( -- * Additive groups
  AdditiveGroup
  -- ** Combinators
, (-)
, (+)
, (×)
, copower
  -- * Additive abelian groups
, AdditiveAbelianGroup
) where


import Data.Functor.Const
import Data.Functor.Identity
import Data.Group
import Data.Int
import Data.Ord
import Data.Proxy
import Data.Semigroup
import Data.Word

import Prelude hiding ((-), (+))

infixl 6 -, +
infixl 7 ×

-- $setup
--
-- >>> import qualified Prelude
-- >>> import Data.Group
-- >>> import Data.Monoid
-- >>> import Data.Semigroup
-- >>> :set -XTypeApplications

-- -------------------------------------------------------------------- --
-- Additive groups

-- | An additive group is a 'Group' whose operation can be thought of
-- as addition in some sense.
--
-- For example, the additive group of integers \( (ℤ, 0, +) \).
--
class Group g => AdditiveGroup g where

instance AdditiveGroup ()
instance AdditiveGroup b => AdditiveGroup (a -> b)
instance AdditiveGroup a => AdditiveGroup (Dual a)
instance AdditiveGroup Any
instance AdditiveGroup (Sum Integer)
instance AdditiveGroup (Sum Rational)
instance AdditiveGroup (Sum Int)
instance AdditiveGroup (Sum Int8)
instance AdditiveGroup (Sum Int16)
instance AdditiveGroup (Sum Int32)
instance AdditiveGroup (Sum Int64)
instance AdditiveGroup (Sum Word)
instance AdditiveGroup (Sum Word8)
instance AdditiveGroup (Sum Word16)
instance AdditiveGroup (Sum Word32)
instance AdditiveGroup (Sum Word64)
instance (AdditiveGroup a, AdditiveGroup b) => AdditiveGroup (a,b)
instance (AdditiveGroup a, AdditiveGroup b, AdditiveGroup c) => AdditiveGroup (a,b,c)
instance (AdditiveGroup a, AdditiveGroup b, AdditiveGroup c, AdditiveGroup d) => AdditiveGroup (a,b,c,d)
instance (AdditiveGroup a, AdditiveGroup b, AdditiveGroup c, AdditiveGroup d, AdditiveGroup e) => AdditiveGroup (a,b,c,d,e)
instance AdditiveGroup a => AdditiveGroup (Const a b)
instance AdditiveGroup a => AdditiveGroup (Identity a)
instance AdditiveGroup a => AdditiveGroup (Proxy a)
instance AdditiveGroup Ordering

-- | Infix alias for 'minus'.
--
-- === __Examples__:
--
-- >>> let x = Sum (3 :: Int)
-- >>> x - x
-- Sum {getSum = 0}
--
-- >>> let x = Any True
-- >>> x - x
-- Any {getAny = True}
--
(-) :: AdditiveGroup a => a -> a -> a
(-) = minus
{-# inline (-) #-}

-- | Infix alias for 'copower'.
--
-- === __Examples__:
--
-- >>> let x = Sum (3 :: Int)
-- >>> 2 × x
-- Sum {getSum = 6}
--
(×) :: (Integral n, AdditiveGroup a) => n -> a -> a
(×) = copower
{-# inline (×) #-}

-- | Infix alias for additive @('<>')@.
--
-- === __Examples__:
--
-- >>> Sum (1 :: Int) + Sum (40 :: Int)
-- Sum {getSum = 41}
--
(+) :: AdditiveGroup g => g -> g -> g
(+) = (<>)
{-# inline (+) #-}

-- | Add an element of an additive group to itself @n@-many times.
--
-- This represents @ℕ@-indexed copowers of an element @g@ of
-- an additive group, i.e. iterated coproducts of group elements.
-- This is representable by the universal property
-- \( C(∐_ₙ g, x) ≅ C(g, x)ⁿ \).
--
-- === __Examples__:
--
-- >>> copower 2 (Sum (3 :: Int))
-- Sum {getSum = 6}
--
copower :: (Integral n, AdditiveGroup g) => n -> g -> g
copower = gtimes
{-# inline copower #-}

-- -------------------------------------------------------------------- --
-- Additive abelian groups

-- | An additive abelian group is an 'AbelianGroup' whose operation can be thought of
-- as commutative addition in some sense. Almost all additive groups are abelian.
--
class (AbelianGroup g, AdditiveGroup g) => AdditiveAbelianGroup g
instance AdditiveAbelianGroup ()
instance AdditiveAbelianGroup b => AdditiveAbelianGroup (a -> b)
instance AdditiveAbelianGroup a => AdditiveAbelianGroup (Dual a)
instance AdditiveAbelianGroup Any
instance AdditiveAbelianGroup (Sum Integer)
instance AdditiveAbelianGroup (Sum Rational)
instance AdditiveAbelianGroup (Sum Int)
instance AdditiveAbelianGroup (Sum Int8)
instance AdditiveAbelianGroup (Sum Int16)
instance AdditiveAbelianGroup (Sum Int32)
instance AdditiveAbelianGroup (Sum Int64)
instance AdditiveAbelianGroup (Sum Word)
instance AdditiveAbelianGroup (Sum Word8)
instance AdditiveAbelianGroup (Sum Word16)
instance AdditiveAbelianGroup (Sum Word32)
instance AdditiveAbelianGroup (Sum Word64)
instance (AdditiveAbelianGroup a, AdditiveAbelianGroup b) => AdditiveAbelianGroup (a,b)
instance (AdditiveAbelianGroup a, AdditiveAbelianGroup b, AdditiveAbelianGroup c) => AdditiveAbelianGroup (a,b,c)
instance (AdditiveAbelianGroup a, AdditiveAbelianGroup b, AdditiveAbelianGroup c, AdditiveAbelianGroup d) => AdditiveAbelianGroup (a,b,c,d)
instance (AdditiveAbelianGroup a, AdditiveAbelianGroup b, AdditiveAbelianGroup c, AdditiveAbelianGroup d, AdditiveAbelianGroup e) => AdditiveAbelianGroup (a,b,c,d,e)
instance AdditiveAbelianGroup a => AdditiveAbelianGroup (Const a b)
instance AdditiveAbelianGroup a => AdditiveAbelianGroup (Identity a)
instance AdditiveAbelianGroup a => AdditiveAbelianGroup (Proxy a)
instance AdditiveAbelianGroup Ordering
