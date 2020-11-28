module Data.Group.Multiplicative
( -- * Multiplicative Groups
  MultiplicativeGroup
  -- ** combinators
, (/)
, (^)
  -- * Multiplicative abelian groups
, MultiplicativeAbelianGroup
) where


import Data.Functor.Const
import Data.Functor.Identity
import Data.Group
import Data.Proxy
import Data.Semigroup

import Prelude hiding ((^), (/))

infixl 7 /
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

class AbelianGroup g => MultiplicativeGroup g where
  power :: Integral n => g -> n -> g
  power a n = stimes n a
  {-# inline power #-}

instance MultiplicativeGroup ()
instance MultiplicativeGroup b => MultiplicativeGroup (a -> b)
instance MultiplicativeGroup a => MultiplicativeGroup (Dual a)
instance MultiplicativeGroup All
instance (Fractional a) => MultiplicativeGroup (Product a)
instance (MultiplicativeGroup a, MultiplicativeGroup b) => MultiplicativeGroup (a,b)
instance (MultiplicativeGroup a, MultiplicativeGroup b, MultiplicativeGroup c) => MultiplicativeGroup (a,b,c)
instance (MultiplicativeGroup a, MultiplicativeGroup b, MultiplicativeGroup c, MultiplicativeGroup d) => MultiplicativeGroup (a,b,c,d)
instance (MultiplicativeGroup a, MultiplicativeGroup b, MultiplicativeGroup c, MultiplicativeGroup d, MultiplicativeGroup e) => MultiplicativeGroup (a,b,c,d,e)
instance MultiplicativeGroup a => MultiplicativeGroup (Const a b)
instance MultiplicativeGroup a => MultiplicativeGroup (Identity a)
instance MultiplicativeGroup a => MultiplicativeGroup (Proxy a)

-- | Infix alias for 'minus'.
--
-- === __Examples__:
--
-- >>> let x = Product (4 :: Rational)
-- >>> x / 2
-- Product {getProduct = 2 % 1}
--
-- >>> let x = All True
-- >>> x / x
-- All {getAll = False}
--
(/) :: MultiplicativeGroup a => a -> a -> a
(/) = minus
{-# inline (/) #-}

-- | Infix alias for 'stimes'.
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

-- -------------------------------------------------------------------- --
-- Multiplicative abelian groups

class (MultiplicativeGroup g, AbelianGroup g) => MultiplicativeAbelianGroup g
instance MultiplicativeAbelianGroup ()
instance MultiplicativeAbelianGroup b => MultiplicativeAbelianGroup (a -> b)
instance MultiplicativeAbelianGroup a => MultiplicativeAbelianGroup (Dual a)
instance MultiplicativeAbelianGroup All
instance (Fractional a) => MultiplicativeAbelianGroup (Product a)
instance (MultiplicativeAbelianGroup a, MultiplicativeAbelianGroup b) => MultiplicativeAbelianGroup (a,b)
instance (MultiplicativeAbelianGroup a, MultiplicativeAbelianGroup b, MultiplicativeAbelianGroup c) => MultiplicativeAbelianGroup (a,b,c)
instance (MultiplicativeAbelianGroup a, MultiplicativeAbelianGroup b, MultiplicativeAbelianGroup c, MultiplicativeAbelianGroup d) => MultiplicativeAbelianGroup (a,b,c,d)
instance (MultiplicativeAbelianGroup a, MultiplicativeAbelianGroup b, MultiplicativeAbelianGroup c, MultiplicativeAbelianGroup d, MultiplicativeAbelianGroup e) => MultiplicativeAbelianGroup (a,b,c,d,e)
instance MultiplicativeAbelianGroup a => MultiplicativeAbelianGroup (Const a b)
instance MultiplicativeAbelianGroup a => MultiplicativeAbelianGroup (Identity a)
instance MultiplicativeAbelianGroup a => MultiplicativeAbelianGroup (Proxy a)
