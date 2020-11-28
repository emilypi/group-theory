{-# language BangPatterns #-}
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
import Data.Monoid

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

instance Num a => Group (Sum a) where
  invert = Prelude.negate
  {-# inline invert #-}

instance Prelude.Fractional a => Group (Product a) where
  invert = Product . Prelude.recip . getProduct
  {-# inline invert #-}

instance (Group a, Group b) => Group (a,b) where
  invert ~(a,b) = (invert a, invert b)
  {-# inline invert #-}

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

-- | Apply @('<>')@, commuting its arguments.
--
-- /Note:/ When the group is abelian, @a <> b@ is identically
-- @b <> a@.
--
(><) :: Group a => a -> a -> a
a >< b = b <> a
{-# inline (><) #-}

-- | Conjugate an element of a group by another element.
--
-- /Note:/ When the group is abelian, conjugation is
-- the identity.
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
-- /Warning:/ elements may be infinite and explode on you if the order
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
instance Num a => AbelianGroup (Sum a)
instance Fractional a => AbelianGroup (Product a)
instance (AbelianGroup a, AbelianGroup b) => AbelianGroup (a,b)
instance (AbelianGroup a, AbelianGroup b, AbelianGroup c) => AbelianGroup (a,b,c)
instance (AbelianGroup a, AbelianGroup b, AbelianGroup c, AbelianGroup d) => AbelianGroup (a,b,c,d)
instance (AbelianGroup a, AbelianGroup b, AbelianGroup c, AbelianGroup d, AbelianGroup e) => AbelianGroup (a,b,c,d,e)
