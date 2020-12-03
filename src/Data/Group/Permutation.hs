{-# language RankNTypes #-}
{-# language Safe #-}
-- |
-- Module       : Data.Group
-- Copyright    : (c) 2020 Reed Mullanix, Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Reed Mullanix <reedmullanix@gmail.com>,
--                Emily Pillmore <emilypi@cohomolo.gy>
--
-- Stability    : stable
-- Portability  : non-portable
--
-- This module provides definitions for 'Permutation's
-- along with useful combinators.
--
module Data.Group.Permutation
( -- * Permutation groups
  Permutation(..)
  -- ** Permutation group combinators
, permute
, pairwise
, (-$)
, ($-)
) where


import Data.Group
import Data.Group.Additive
import Data.Group.Multiplicative

infixr 0 $-, -$

-- -------------------------------------------------------------------- --
-- Permutations

-- | Isomorphism of a finite set onto itself. Each entry consists of one
-- half of the isomorphism.
--
-- /Note/: It is the responsibility of the user to provide inverse proofs
-- for 'to' and 'from'. Be responsible!
--
data Permutation a = Permutation
  { to :: a -> a
  , from :: a -> a
  }

-- instance Profunctor Permutation where
--   dimap = :'(

instance Semigroup a => Semigroup (Permutation a) where
  a <> b = Permutation (to a <> to b) (from a <> from b)

instance Monoid a => Monoid (Permutation a) where
  mempty = Permutation id id

instance Group a => Group (Permutation a) where
  invert p = Permutation (invert . to p) (from p . invert)

instance AbelianGroup a => AbelianGroup (Permutation a)
instance AdditiveGroup a => AdditiveGroup (Permutation a)
instance AdditiveAbelianGroup a => AdditiveAbelianGroup (Permutation a)
instance MultiplicativeGroup a => MultiplicativeGroup (Permutation a)

-- -------------------------------------------------------------------- --
-- Permutation group combinators

-- | Build a 'Permutation' from a bijective pair.
--
permute :: (a -> a) -> (a -> a) -> Permutation a
permute = Permutation
{-# inline permute #-}

-- | Destroy a 'Permutation', producing the underlying pair of
-- bijections.
--
pairwise :: Permutation a -> (a -> a, a -> a)
pairwise p = (to p, from p)
{-# inline pairwise #-}

-- | Infix alias for the 'to' half of 'Permutation' bijection
--
(-$) :: Permutation a -> a -> a
(-$) = to
{-# inline (-$) #-}

-- | Infix alias for the 'from' half of 'Permutation' bijection
--
($-) :: Permutation a -> a -> a
($-) = from
{-# inline ($-) #-}
