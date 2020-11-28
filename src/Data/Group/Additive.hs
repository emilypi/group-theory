module Data.Group.Additive
( -- * Additive groups
  AdditiveGroup
  -- ** Combinators
, (-)
, (×)
  -- * Additive abelian groups
, AdditiveAbelianGroup
) where


import Data.Group
import Data.Semigroup

import Prelude hiding ((-))

infixl 6 -
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

class AbelianGroup g => AdditiveGroup g
instance AdditiveGroup ()
instance AdditiveGroup b => AdditiveGroup (a -> b)
instance AdditiveGroup a => AdditiveGroup (Dual a)
instance AdditiveGroup Any
instance Num a => AdditiveGroup (Sum a)
instance (AdditiveGroup a, AdditiveGroup b) => AdditiveGroup (a,b)
instance (AdditiveGroup a, AdditiveGroup b, AdditiveGroup c) => AdditiveGroup (a,b,c)
instance (AdditiveGroup a, AdditiveGroup b, AdditiveGroup c, AdditiveGroup d) => AdditiveGroup (a,b,c,d)
instance (AdditiveGroup a, AdditiveGroup b, AdditiveGroup c, AdditiveGroup d, AdditiveGroup e) => AdditiveGroup (a,b,c,d,e)


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

-- | Add an element to itself @n@-many times.
--
-- === __Examples__:
--
-- >>> let x = Sum (3 :: Int)
-- >>> 2 × x
-- Sum {getSum = 6}
--
(×) :: (Integral n, AdditiveGroup a) => n -> a -> a
n × a = stimes n a

-- -------------------------------------------------------------------- --
-- Additive abelian groups

class (AbelianGroup g, AdditiveGroup g) => AdditiveAbelianGroup g
instance AdditiveAbelianGroup ()
instance AdditiveAbelianGroup b => AdditiveAbelianGroup (a -> b)
instance AdditiveAbelianGroup a => AdditiveAbelianGroup (Dual a)
instance AdditiveAbelianGroup Any
instance Num a => AdditiveAbelianGroup (Sum a)
instance (AdditiveAbelianGroup a, AdditiveAbelianGroup b) => AdditiveAbelianGroup (a,b)
instance (AdditiveAbelianGroup a, AdditiveAbelianGroup b, AdditiveAbelianGroup c) => AdditiveAbelianGroup (a,b,c)
instance (AdditiveAbelianGroup a, AdditiveAbelianGroup b, AdditiveAbelianGroup c, AdditiveAbelianGroup d) => AdditiveAbelianGroup (a,b,c,d)
instance (AdditiveAbelianGroup a, AdditiveAbelianGroup b, AdditiveAbelianGroup c, AdditiveAbelianGroup d, AdditiveAbelianGroup e) => AdditiveAbelianGroup (a,b,c,d,e)
