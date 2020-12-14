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
-- This module provides definitions for Church-encoded
-- 'FreeGroup's, 'FreeAbelianGroup's, along with useful combinators.
--
module Data.Group.Free.Church
( -- * Church-encoded free groups
  FG(..)
  -- ** Church-encoded free group combinators
, interpretFG
, reifyFG
, reflectFG
, presentFG
  -- * Church-encoded free abelian groups
, FA(..)
  -- ** Church-encoded free abelian group combinators
, forgetFA
, interpretFA
, reifyFA
, reflectFA
) where

import Control.Applicative
import Control.Monad

import Data.Semigroup(Semigroup(..))
import Data.Group
import Data.Group.Free
import qualified Data.Map.Strict as Map

-- | The Church-encoding of a 'FreeGroup'.
--
-- This datatype represents the "true" free group in Haskell on some @a@-valued
-- generators. For more information on why this encoding is preferred,
-- see Dan Doel's <http://comonad.com/reader/2015/free-monoids-in-haskell/ article> in
-- the Comonad Reader.
--
-- While 'FreeGroup' et al are free in a strict language, and are more intuitive,
-- they are not associative wtih respect to bottoms. 'FG' and 'FA' however, are,
-- and should be preferred when working with possibly undefined data.
--
newtype FG a = FG { runFG :: forall g. Group g => (a -> g) -> g }

instance Semigroup (FG a) where
  (FG g) <> (FG g') = FG $ \k -> g k <> g' k

instance Monoid (FG a) where
  mempty = FG $ const mempty

instance Group (FG a) where
  invert (FG g) = FG (invert . g)

instance Functor FG where
  fmap f (FG fa) = FG $ \k -> fa (k . f)

instance Applicative FG where
  pure a = FG ($ a)
  (<*>) = ap

instance Monad FG where
  return = pure
  (FG fg) >>= f = FG $ \k -> fg (\a -> (runFG $ f a) k)

instance Alternative FG where
  empty = mempty
  (<|>) = (<>)

-- | Interpret a Church-encoded free group as a concrete 'FreeGroup'.
--
interpretFG :: Group g => FG g -> g
interpretFG (FG fg) = fg id
{-# inline interpretFG #-}

-- | Convert a Church-encoded free group to a concrete 'FreeGroup'.
--
reifyFG :: FG a -> FreeGroup a
reifyFG fg = interpretFG $ fmap pure fg
{-# inline reifyFG #-}

-- | Convert a concrete 'FreeGroup' to a Church-encoded free group.
--
reflectFG :: FreeGroup a -> FG a
reflectFG (FreeGroup fg) = FG $ \k -> foldMap (go k) fg
  where
    go k (Left a) = invert (k a)
    go k (Right a) = k a
{-# inline reflectFG #-}

-- | Present a 'Group' as a 'FG' modulo relations.
--
presentFG :: Group g => FG g -> (FG g -> g) -> g
presentFG = flip ($)
{-# inline presentFG #-}

----------------------------------------
-- Free Abelian Groups

-- | The Church-encoding of a 'FreeAbelianGroup'.
--
-- This datatype represents the free group on some @a@-valued
-- generators, along with their exponents in the group.
--
newtype FA a = FA { runFA :: forall g. Abelian g => (a -> Integer -> g) -> g }

instance Semigroup (FA a) where
  (FA g) <> (FA g') = FA $ \k -> g k <> g' k
  stimes = gtimes

instance Monoid (FA a) where
  mempty = FA $ const mempty

instance Group (FA a) where
  invert g = pow g (-1 :: Integer)

  {-
  Note: This implementation "optimizes" from the default implementation of
  'pow', or more natural

  > pow (FA g) n = FA $ \k -> gtimes n (g k)

  by delaying the call of 'gtimes' as late as possible.

  This is only possible because we expect 'Group g' to be an abelian group,
  which implies the following equation hold:

  > pow (x <> y) n = pow x n <> pow y n
  -}
  pow (FA g) n
    | n == 0    = mempty
    | otherwise = FA $ \k -> g (\a i -> k a (toInteger n * i))

instance Abelian (FA a)

instance Functor FA where
  fmap f (FA fa) = FA $ \k -> fa (k . f)

instance Applicative FA where
  pure a = FA $ \k -> k a 1
  (<*>) = ap

instance Monad FA where
  return = pure
  fa >>= f = interpretFA $ fmap f fa

instance Alternative FA where
  empty = mempty
  (<|>) = (<>)

-- | Interpret a Church-encoded free abelian group as a concrete 'FreeAbelianGroup'.
--
interpretFA :: Abelian g => FA g -> g
interpretFA (FA fa) = fa pow
{-# inline interpretFA #-}

-- | Convert a Church-encoded free abelian group to a concrete 'FreeAbelianGroup'.
--
reifyFA :: Ord a => FA a -> FreeAbelianGroup a
reifyFA = interpretFA . fmap singleton
{-# inline reifyFA #-}

-- | Convert a concrete 'FreeAbelianGroup' to a Church-encoded free abelian group.
--
reflectFA :: FreeAbelianGroup a -> FA a
reflectFA fa =
  let g = runFreeAbelianGroup fa
  in FA $ \k -> Map.foldMapWithKey k g
{-# inline reflectFA #-}

-- | Forget the commutative structure of a Church-encoded free abelian group,
-- turning it into a standard free group.
forgetFA :: (Ord a) => FA a -> FG a
forgetFA fa = case reifyFA fa of
  ~(FreeAbelianGroup fa') -> FG $ \t -> Map.foldMapWithKey (\a n -> t a `pow` n) fa'
{-# inline forgetFA #-}
