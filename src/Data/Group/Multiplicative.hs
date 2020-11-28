module Data.Group.Multiplicative
( -- * Multiplicative Groups
  MultiplicativeGroup
  -- ** combinators
, (/)
, (^)
) where


import Data.Group
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

class AbelianGroup g => MultiplicativeGroup g
instance MultiplicativeGroup ()
instance MultiplicativeGroup b => MultiplicativeGroup (a -> b)
instance MultiplicativeGroup a => MultiplicativeGroup (Dual a)
instance MultiplicativeGroup All
instance (Fractional a) => MultiplicativeGroup (Product a)
instance (MultiplicativeGroup a, MultiplicativeGroup b) => MultiplicativeGroup (a,b)
instance (MultiplicativeGroup a, MultiplicativeGroup b, MultiplicativeGroup c) => MultiplicativeGroup (a,b,c)
instance (MultiplicativeGroup a, MultiplicativeGroup b, MultiplicativeGroup c, MultiplicativeGroup d) => MultiplicativeGroup (a,b,c,d)
instance (MultiplicativeGroup a, MultiplicativeGroup b, MultiplicativeGroup c, MultiplicativeGroup d, MultiplicativeGroup e) => MultiplicativeGroup (a,b,c,d,e)


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
(^) :: (Integral n, Group a) => a -> n -> a
a ^ n = stimes n a
{-# inline (^) #-}
