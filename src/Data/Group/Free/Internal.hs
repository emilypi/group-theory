{-# language Unsafe #-}
-- |
-- Module       : Data.Group.Free.Internal
-- Copyright    : (c) 2020 Reed Mullanix, Emily Pillmore, Koji Miyazato
-- License      : BSD-style
--
-- Maintainer   : Reed Mullanix <reedmullanix@gmail.com>,
--                Emily Pillmore <emilypi@cohomolo.gy>
--
-- Stability    : stable
-- Portability  : non-portable
--
-- This module exposes internals of 'FreeAbelianGroup'.
--
module Data.Group.Free.Internal
( -- * Free abelian groups
  FreeAbelianGroup(..)
) where


import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map

import Data.Semigroup(Semigroup(..))
import Data.Group
import Data.Group.Order


-- $setup
--
-- >>> import qualified Prelude
-- >>> import Data.Group
-- >>> import Data.Monoid
-- >>> import Data.Semigroup
-- >>> import Data.Word
-- >>> :set -XTypeApplications
-- >>> :set -XFlexibleContexts

-- | A representation of a free abelian group over an alphabet @a@.
--
-- The intuition here is group elements correspond with their positive
-- or negative multiplicities, and as such are simplified by construction.
--
-- === __Examples__:
--
-- >>> let single a = MkFreeAbelianGroup $ Map.singleton a 1
-- >>> a = single 'a'
-- >>> b = single 'b'
-- >>> a
-- FreeAbelianGroup $ fromList [('a',1)]
-- >>> a <> b
-- FreeAbelianGroup $ fromList [('a',1),('b',1)]
-- >>> a <> b == b <> a
-- True
-- >>> invert a
-- FreeAbelianGroup $ fromList [('a',-1)]
-- >>> a <> b <> invert a
-- FreeAbelianGroup $ fromList [('b',1)]
-- >>> gtimes 5 (a <> b)
-- FreeAbelianGroup $ fromList [('a',5),('b',5)]
--
newtype FreeAbelianGroup a =
  MkFreeAbelianGroup (Map a Integer)
    -- ^ Unsafe "raw" constructor, which does not do normalization work.
    -- Please use 'Data.Group.Free.mkFreeAbelianGroup' as it normalizes.
    --
  deriving (Eq, Ord)

instance Show a => Show (FreeAbelianGroup a) where
    showsPrec p (MkFreeAbelianGroup g) =
        showParen (p > 0) $ ("FreeAbelianGroup $ " ++) . shows g

instance (Ord a) => Semigroup (FreeAbelianGroup a) where
    (MkFreeAbelianGroup g) <> (MkFreeAbelianGroup g') =
      MkFreeAbelianGroup $ mergeG g g'
      where
        mergeG = Map.merge
          Map.preserveMissing
          Map.preserveMissing
          (Map.zipWithMaybeMatched $ \_ m n -> nonZero $ m + n)
        nonZero n = if n == 0 then Nothing else Just n

    stimes = flip pow

instance (Ord a) => Monoid (FreeAbelianGroup a) where
    mempty = MkFreeAbelianGroup Map.empty

instance (Ord a) => Group (FreeAbelianGroup a) where
    invert (MkFreeAbelianGroup g) = MkFreeAbelianGroup $ Map.map negate g

    pow _ 0 = mempty
    pow (MkFreeAbelianGroup g) n
      | n == 0    = mempty
      | otherwise = MkFreeAbelianGroup $ Map.map (toInteger n *) g

instance (Ord a) => Abelian (FreeAbelianGroup a)

instance (Ord a) => GroupOrder (FreeAbelianGroup a) where
    order g | g == mempty = Finite 1
            | otherwise   = Infinite
