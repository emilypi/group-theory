{-# language PatternSynonyms #-}
{-# language Safe #-}
{-# language ViewPatterns #-}
-- |
-- Module       : Data.Group
-- Copyright    : (c) 2020 Emily Pillmore
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
, embed
, retract
  -- ** Permutation patterns
, pattern Permute
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
    -- ^ The forward half of the bijection
  , from :: a -> a
    -- ^ The inverse half of the bijection
  }

-- instance Profunctor Permutation where
--   dimap = :'(

instance Semigroup a => Semigroup (Permutation a) where
  a <> b = Permutation (to a <> to b) (from a <> from b)

instance Monoid a => Monoid (Permutation a) where
  mempty = Permutation id id

instance Group a => Group (Permutation a) where
  invert (Permutation t f) = Permutation (f . t) (t . f)

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

-- | Embed a 'Group' into the 'Permutation' group on it's underlying set.
--
embed :: (Group g) => g -> Permutation g
embed g = Permutation { to = (g <>), from = (invert g <>) }

-- | Get a group element out of the permutation group.
-- This is a left inverse to 'embed', i.e.
--
-- @
--    retract . embed = id
-- @
--
retract :: (Group g) => Permutation g -> g
retract p = p -$ mempty

-- | Bidirectional pattern synonym for embedding/retraction of groups
-- into their permutation groups.
--
pattern Permute :: Group g => Permutation g -> g
pattern Permute p <- (embed -> p)
  where Permute p = retract p
