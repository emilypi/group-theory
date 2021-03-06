{-# language BangPatterns #-}
{-# language PatternSynonyms #-}
{-# language Safe #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language ViewPatterns #-}
-- |
-- Module       : Data.Group
-- Copyright    : (c) 2020-2021 Emily Pillmore
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
, equalPermutation
, comparePermutation
, orderOfPermutation
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

import qualified Data.IntSet as ISet
import Data.Function (on)
import Numeric.Natural (Natural)

infixr 0 $-, -$

-- -------------------------------------------------------------------- --
-- Permutations

-- | Isomorphism of a type onto itself. Each entry consists of one
-- half of the isomorphism.
--
-- /Note/: It is the responsibility of the user to provide inverse proofs
-- for 'to' and 'from'. Be responsible!
--
-- === __Examples:__
--
-- >>> p1 = permute succ pred :: Permutation Integer
-- >>> p2 = permute negate negate :: Permutation Integer
-- >>> to (p1 <> p2) 2
-- -1
-- >>> from (p1 <> p2) (-1)
-- 2
-- >>> to (p2 <> p1) 2
-- -3
--
-- Permutations on a finite set @a@ (, indicated by satisfying
-- @(Bounded a, Enum a)@ constraint,) can be tested their equality
-- and computed their 'order's.
--
-- >>> c1 = permute not not :: Permutation Bool
-- >>> equalPermutation (c1 <> c1) mempty
-- True
-- >>> orderOfPermutation c1
-- 2
data Permutation a = Permutation
  { to :: a -> a
    -- ^ The forward half of the bijection
  , from :: a -> a
    -- ^ The inverse half of the bijection
  }

-- instance Profunctor Permutation where
--   dimap = :'(

instance Semigroup (Permutation a) where
  a <> b = Permutation (to a . to b) (from b . from a)

instance Monoid (Permutation a) where
  mempty = Permutation id id

instance Group (Permutation a) where
  invert (Permutation t f) = Permutation f t

-- | Equality test for permutations on a finite type 'a'
--
-- This module /intentionally omits/ the following instance, albeit
-- 'equalPermutation' is suitable implementation of
-- @(==)@ operator for many types.
--
-- > instance (Enum a, Bounded a) => Eq (Permutation a) where
-- >   (==) = equalPermutation
--
-- This is because some type satisfying @(Enum a, Bounded a)@ are
-- actually finite but too large to use @equalPermutation@ on.
-- For example, you can call @equalPermutation@ on @Permutation Int@,
-- but it takes too much computation to be considered usable.
equalPermutation
  :: (Enum a, Bounded a) => Permutation a -> Permutation a -> Bool
equalPermutation = (==) `on` (functionRepr . to)

-- | Comparison for permutations on a finite type 'a'
--
-- This module /intentionally omits/ the following instance, albeit
-- 'comparePermutation' is suitable implementation of
-- @compare@ method for many types.
--
-- > instance (Enum a, Bounded a) => Eq (Permutation a) where
-- >   compare = comparePermutation
--
-- This is because some type satisfying @(Enum a, Bounded a)@ are
-- actually finite but too large to use @comparePermutation@ on.
-- For example, you can call @comparePermutation@ on @Permutation Int@,
-- but it takes too much computation to be considered usable.
comparePermutation
  :: (Enum a, Bounded a) => Permutation a -> Permutation a -> Ordering
comparePermutation = compare `on` (functionRepr . to)

functionRepr :: (Enum a, Bounded a) => (a -> a) -> [Int]
functionRepr f = fromEnum . f <$> [minBound .. maxBound]

-- | Order counting for a permutation on a finite type 'a'
--
-- This module /intentionally omits/ the following instance, albeit
-- 'orderOfPermutation' is suitable implementation of
-- @order@ method for many types.
--
-- > instance (Enum a, Bounded a) => GroupOrder (Permutation a) where
-- >   order a = Finite (orderOfPermutation a)
--
-- This is because some type satisfying @(Enum a, Bounded a)@ are
-- actually finite but too large to use @orderOfPermutation@ on.
-- For example, you can call @orderOfPermutation@ on @Permutation Int@,
-- but it takes too much computation to be considered usable.
orderOfPermutation
  :: forall a. (Enum a, Bounded a) => Permutation a -> Natural
orderOfPermutation Permutation{to = f} = go 1 fullSet
    where
      n = 1 + fromEnum (maxBound @a)
      fullSet = ISet.fromDistinctAscList [0 .. n - 1]

      f' :: Int -> Int
      f' = fromEnum . f . toEnum

      go :: Natural -> ISet.IntSet -> Natural
      go !ord elements = case ISet.minView elements of
        Nothing             -> ord
        Just (k, elements') ->
          let (period, elements'') = takeCycle k elements'
          in go (lcm period ord) elements''

      takeCycle :: Int -> ISet.IntSet -> (Natural, ISet.IntSet)
      takeCycle k = loop 1 (f' k)
        where
          loop !period j elements
            | j `ISet.member` elements      = loop (succ period) (f' j) (ISet.delete j elements)
            | {- j ∉ elements && -} j == k = (period, elements)
            | otherwise                     = error $ "Non-bijective: witness=toEnum " ++ show j

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
