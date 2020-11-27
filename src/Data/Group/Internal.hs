module Data.Group.Internal
( -- * Groups
  Group(..)
, (-)
, WrappedGroup(..)
  -- * Abelian groups
, AbelianGroup
, commuting
) where


import Data.Bool
import Data.Monoid
import Data.Semigroup

import Prelude hiding ((-), negate)
import qualified Prelude


infixl 6 -


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

(-) :: Group a => a -> a -> a
(-) = minus

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
-- Wrapped groups

newtype WrappedGroup a = WrappedGroup a

instance Group a => Semigroup (WrappedGroup a) where
  WrappedGroup a <> WrappedGroup b = WrappedGroup (a <> b)

instance Group a => Monoid (WrappedGroup a) where
  mempty = WrappedGroup mempty

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

-- | Apply @('<>')@ commuting arguments.
--
commuting :: AbelianGroup a => a -> a -> a
commuting a b = b <> a
