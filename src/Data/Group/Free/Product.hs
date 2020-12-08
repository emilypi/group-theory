-- |
-- Module       : Data.Group.Free.Product
-- Copyright    : (c) 2020 Reed Mullanix, Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Reed Mullanix <reedmullanix@gmail.com>,
--                Emily Pillmore <emilypi@cohomolo.gy>
--
-- Stability    : stable
-- Portability  : non-portable
--
-- This module provides definitions for the 'FreeProduct' of two groups,
-- along with useful combinators.
--
module Data.Group.Free.Product
( FreeProduct(..)
, simplify
, coproduct
, injl
, injr
) where

import Data.Bifunctor
import Data.Group

import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq


-- -------------------------------------------------------------------- --
-- Free products

-- | The free product on two alphabets
--
-- __Note:__ This does not perform simplification upon multiplication or construction.
-- To do this, one should use 'simplify'.
--
newtype FreeProduct g h = FreeProduct { runFreeProduct :: Seq (Either g h) }
  deriving (Show, Eq, Ord)

instance Functor (FreeProduct g) where
  fmap f = FreeProduct . fmap (fmap f) . runFreeProduct

instance Bifunctor FreeProduct where
  bimap f g = FreeProduct . fmap (bimap f g) . runFreeProduct

-- | /O(n)/ Simplifies a word in a 'FreeProduct'.
-- This means that we get rid of any identity elements, and perform multiplication of neighboring @g@s and @h@s.
--
simplify :: (Eq g, Eq h, Monoid g, Monoid h) => FreeProduct g h -> FreeProduct g h
simplify (FreeProduct fp) = FreeProduct $ go fp
  where
    go (Left IdentityElem :<| ghs) = go ghs
    go (Right IdentityElem :<| ghs) = go ghs
    go (Left g :<| Left g' :<| ghs) = go $ Left (g <> g') :<| ghs
    go (Right h :<| Right h' :<| ghs) = go $ Right (h <> h') :<| ghs
    go (gh :<| ghs) = gh :<| go ghs
    go Empty = Empty

instance Semigroup (FreeProduct g h) where
  FreeProduct ghs <> FreeProduct ghs' = FreeProduct $ ghs <> ghs'

instance Monoid (FreeProduct g h) where
  mempty = FreeProduct Seq.empty

instance (Group g, Group h) => Group (FreeProduct g h) where
  invert (FreeProduct ghs) = FreeProduct $ bimap invert invert <$> Seq.reverse ghs

-- | Left injection of an alphabet @a@ into a free product.
--
injl :: a -> FreeProduct a b
injl a = FreeProduct $ Seq.singleton (Left a)

-- | Right injection of an alphabet @b@ into a free product.
--
injr :: b -> FreeProduct a b
injr b = FreeProduct $ Seq.singleton (Right b)

-- | The 'FreeProduct' of two 'Monoid's is a coproduct in the category of monoids (and by extension, the category of groups).
--
coproduct :: Monoid m => (a -> m) -> (b -> m) -> FreeProduct a b -> m
coproduct gi hi (FreeProduct ghs) = foldMap (either gi hi) ghs
