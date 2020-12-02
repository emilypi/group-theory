{-# language RankNTypes #-}
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
-- This module provides definitions for Church-encoded
-- 'FreeGroup's, 'FreeAbelianGroup's, along with useful combinators.
--
module Data.Group.Free.Church where

import Control.Applicative
import Control.Monad

import Data.Group
import Data.Group.Free
import qualified Data.Map.Strict as Map
import Data.Functor.Compose

-- FIXME: Good name pls
newtype FG a = FG { unFG :: forall g. (Group g) => (a -> g) -> g }

instance Semigroup (FG a) where
    (FG g) <> (FG g') = FG $ \k -> g k <> g' k

instance Monoid (FG a) where
    mempty = FG $ \_ -> mempty

instance Group (FG a) where
    invert (FG g) = FG $ \k -> invert $ g k

instance Functor FG where
    fmap f (FG fa) = FG $ \k -> fa (k . f)

instance Applicative FG where
    pure a = FG $ \k -> k a
    (<*>) = ap

instance Monad FG where
    return = pure
    (FG fg) >>= f = FG $ \k -> fg (\a -> (unFG $ f a) k)

instance Alternative FG where
    empty = mempty
    (<|>) = (<>)

-- | Interpret a Church-encoded free group as a concrete 'FreeGroup'.
--
interpretFG :: Group g => FG g -> g
interpretFG (FG fg) = fg id

-- | Convert a Church-encoded free group to a concrete 'FreeGroup'.
--
reifyFG :: FG a -> FreeGroup a
reifyFG fg = interpretFG $ fmap pure fg

-- | Convert a concrete 'FreeGroup' to a Church-encoded free group.
--
reflectFG :: FreeGroup a -> FG a
reflectFG (FreeGroup fg) = FG $ \k -> foldMap k (Compose fg)

-- | Present a 'Group' as a 'FreeGroup' modulo relations.
--
present :: (Group g) => FG g -> (FreeGroup g -> g) -> g
present fg p = p $ reifyFG fg

----------------------------------------
-- Free Abelian Groups

-- FIXME: Good name pls
-- :)
newtype FA a = FA { unFA :: forall g. (Group g) => (a -> Int -> g) -> g }

instance Semigroup (FA a) where
    (FA g) <> (FA g') = FA $ \k -> g k <> g' k

instance Monoid (FA a) where
    mempty = FA $ \_ -> mempty

instance Group (FA a) where
    invert (FA g) = FA $ \k -> invert $ g k

-- Contravariant pls!!
instance Functor FA where
    fmap f (FA fa) = FA $ \k -> fa (\a n -> k (f a) n)

instance Applicative FA where
    pure a = FA $ \k -> k a 1
    (<*>) = ap

instance Monad FA where
    return = pure
    (FA fa) >>= f = FA $ \k -> fa (\a n -> gtimes n $ (unFA $ f a) k)

instance Alternative FA where
    empty = mempty
    (<|>) = (<>)

-- | Interpret a Church-encoded free abelian group as a concrete 'FreeAbelianGroup'.
--
interpretFA :: Group g => FA g -> g
interpretFA (FA fa) = fa (flip gtimes)

-- | Convert a Church-encoded free abelian group to a concrete 'FreeAbelianGroup'.
--
reifyFA :: (Ord a) => FA a -> FreeAbelian a
reifyFA fa = interpretFA $ fmap singleton fa

-- | Convert a concrete 'FreeAbelianGroup' to a Church-encoded free abelian group.
--
reflectFA :: (Ord a) => FreeAbelian a -> FA a
reflectFA (FreeAbelian fa) = FA $ \k -> Map.foldMapWithKey k fa
