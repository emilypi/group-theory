{-# language Safe #-}
-- |
-- Module       : Data.Group
-- Copyright    : (c) 2020 Reed Mullanix
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
  ( FreeGroup(..)
  , GroupFoldable(..)
  , simplify
  , interpret
  , interpret'
  , FreeAbelian(..)
  , abMap
  , abJoin
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


class GroupFoldable t where
    gold :: (Group g) => t g -> g
    gold = goldMap id

    goldMap :: (Group g) => (a -> g) -> t a -> g

    toFreeGroup :: t a -> FreeGroup a
    -- goldr :: (a -> Iso a a) -> b -> t a -> a
    {-# minimal goldMap | toFreeGroup #-}

-- | A representation of a free group over an alphabet @a@.
-- The intuition here is that @Left a@ represents a "negative" @a@,
-- whereas @Right a@ represents "positive" @a@.
--
-- __Note:__ This does not perform simplification upon multiplication or construction.
-- To do this, one should use 'simplify'.
--
newtype FreeGroup a = FreeGroup { unFreeGroup :: [Either a a] }
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
          go (Left a)  = unFreeGroup $ invert $ f a
          go (Right a) = unFreeGroup $ f a

instance Alternative FreeGroup where
    empty = mempty
    (<|>) = (<>)
-- | /O(n)/ Simplifies a word in a free group.
--
-- === __Examples:__
--
-- >>> simplify $ FreeGroup $ [Right 'a', Left 'b', Right 'c', Left 'c', Right 'b', Right 'a']
-- FreeGroup {unFreeGroup = [Right 'a',Right 'a']}
simplify :: (Eq a) => FreeGroup a -> FreeGroup a
simplify (FreeGroup g) = FreeGroup $ foldr go [] g
    where
      go (Left a) ((Right a'):as) | a == a' = as
      go (Right a) ((Left a'):as) | a == a' = as
      go a as = a:as

-- | /O(n)/ Interpret a word in a free group over some group @g@ as an element in a group @g@.
interpret :: (Group g) => FreeGroup g -> g
interpret (FreeGroup g) = foldr go mempty g
    where
      go (Left a) acc  = invert a <> acc
      go (Right a) acc = a <> acc

-- | /O(n)/ Strict variant of 'interpret'.
interpret' :: (Group g) => FreeGroup g -> g
interpret' (FreeGroup g) = foldl' go mempty g
    where
      go acc (Left a) = acc <> invert a
      go acc (Right a) = acc <> a

newtype FreeAbelian a = FreeAbelian { unFreeAbelian :: Map a Int }
    deriving (Show, Eq, Ord)

instance (Ord a) => Semigroup (FreeAbelian a) where
    (FreeAbelian g) <> (FreeAbelian g') = FreeAbelian $ Map.unionWith (+) g g'

instance (Ord a) => Monoid (FreeAbelian a) where
    mempty = FreeAbelian mempty

instance (Ord a) => Group (FreeAbelian a) where
    invert (FreeAbelian g) = FreeAbelian $ fmap negate g

-- NOTE: We can't implement Functor/Applicative/Monad here
-- due to the Ord constraint. C'est La Vie!

abMap :: (Ord b) => (a -> b) -> FreeAbelian a -> FreeAbelian b
abMap f (FreeAbelian g) = FreeAbelian $ Map.mapKeys f g

singleton :: a -> FreeAbelian a
singleton a = FreeAbelian $ Map.singleton a 1

abJoin :: (Ord a) => FreeAbelian (FreeAbelian a) -> FreeAbelian a
abJoin (FreeAbelian g) = FreeAbelian $ Map.foldMapWithKey go g
    where
      go (FreeAbelian g') n = fmap (*n) g'

abInterpret :: (Group g) => FreeAbelian g -> g
abInterpret (FreeAbelian g) = Map.foldMapWithKey (flip gtimes) g
