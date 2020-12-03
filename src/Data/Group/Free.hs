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
-- This module provides definitions for 'FreeGroup's, 'FreeAbelianGroup's,
-- and 'GroupFoldable', along with useful combinators.
--
module Data.Group.Free
( -- * Free groups
  FreeGroup(..)
  -- ** Free group combinators
, simplify
, interpret
, interpret'
  -- * Free abelian groups
, FreeAbelianGroup(..)
  -- ** Free abelian group combinators
, abmap
, abjoin
, singleton
, abInterpret
) where

import Control.Applicative
import Control.Monad

import Data.Bifunctor
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Group


-- | A representation of a free group over an alphabet @a@.
--
-- The intuition here is that @Left a@ represents a "negative" @a@,
-- whereas @Right a@ represents "positive" @a@.
--
-- __Note:__ This does not perform simplification upon multiplication or construction.
-- To do this, one should use 'simplify'.
--
newtype FreeGroup a = FreeGroup { runFreeGroup :: [Either a a] }
    deriving (Show, Eq, Ord)

instance Semigroup (FreeGroup a) where
    (FreeGroup g) <> (FreeGroup g') = FreeGroup (g ++ g')

instance Monoid (FreeGroup a) where
    mempty = FreeGroup []

instance Group (FreeGroup a) where
    invert (FreeGroup g) = FreeGroup $ fmap inv g
        where
          inv :: Either a a -> Either a a
          inv (Left a) = Right a
          inv (Right a) = Left a

instance Functor FreeGroup where
    fmap f (FreeGroup g) = FreeGroup $ fmap (bimap f f) g

instance Applicative FreeGroup where
    pure a = FreeGroup $ pure $ pure a
    (<*>) = ap

instance Monad FreeGroup where
    return = pure
    (FreeGroup g) >>= f = FreeGroup $ concatMap go g
        where
          go (Left a)  = runFreeGroup $ invert $ f a
          go (Right a) = runFreeGroup $ f a

instance Alternative FreeGroup where
    empty = mempty
    (<|>) = (<>)

-- | /O(n)/ Simplifies a word in a free group.
--
-- === __Examples:__
--
-- >>> simplify $ FreeGroup $ [Right 'a', Left 'b', Right 'c', Left 'c', Right 'b', Right 'a']
-- FreeGroup {unFreeGroup = [Right 'a',Right 'a']}
--
simplify :: (Eq a) => FreeGroup a -> FreeGroup a
simplify (FreeGroup g) = FreeGroup $ foldr go [] g
    where
      go (Left a) ((Right a'):as) | a == a' = as
      go (Right a) ((Left a'):as) | a == a' = as
      go a as = a:as

-- | /O(n)/ Interpret a word in a free group over some group @g@ as an element in a group @g@.
--
interpret :: (Group g) => FreeGroup g -> g
interpret (FreeGroup g) = foldr go mempty g
    where
      go (Left a) acc  = invert a <> acc
      go (Right a) acc = a <> acc

-- | /O(n)/ Strict variant of 'interpret'.
--
interpret' :: (Group g) => FreeGroup g -> g
interpret' (FreeGroup g) = foldl' go mempty g
    where
      go acc (Left a) = acc <> invert a
      go acc (Right a) = acc <> a

-- | A representation of a free abelian group over an alphabet @a@.
--
-- The intuition here is group elements correspond with their positive
-- or negative multiplicities, and as such are simplified by construction.
--
newtype FreeAbelianGroup a = FreeAbelianGroup { runFreeAbelian :: Map a Int }
    deriving (Show, Eq, Ord)

instance (Ord a) => Semigroup (FreeAbelianGroup a) where
    (FreeAbelianGroup g) <> (FreeAbelianGroup g') =
      FreeAbelianGroup $ Map.unionWith (+) g g'

instance (Ord a) => Monoid (FreeAbelianGroup a) where
    mempty = FreeAbelianGroup mempty

instance (Ord a) => Group (FreeAbelianGroup a) where
    invert (FreeAbelianGroup g) = FreeAbelianGroup $ fmap negate g

-- NOTE: We can't implement Functor/Applicative/Monad here
-- due to the Ord constraint. C'est La Vie!

-- | Functorial 'fmap' for a 'FreeAbelian' group.
--
abmap :: (Ord b) => (a -> b) -> FreeAbelianGroup a -> FreeAbelianGroup b
abmap f (FreeAbelianGroup g) = FreeAbelianGroup $ Map.mapKeys f g

-- | Lift a singular value into a 'FreeAbelianGroup'. Analogous to 'pure'.
--
singleton :: a -> FreeAbelianGroup a
singleton a = FreeAbelianGroup $ Map.singleton a 1

-- | Monadic 'join' for a 'FreeAbelian' group.
--
abjoin :: (Ord a) => FreeAbelianGroup (FreeAbelianGroup a) -> FreeAbelianGroup a
abjoin (FreeAbelianGroup g) = FreeAbelianGroup $ Map.foldMapWithKey go g
    where
      go (FreeAbelianGroup g') n = fmap (*n) g'

-- | Interpret a free group as a word in the underlying group @g@.
--
abInterpret :: (Group g) => FreeAbelianGroup g -> g
abInterpret (FreeAbelianGroup g) = Map.foldMapWithKey (flip gtimes) g
