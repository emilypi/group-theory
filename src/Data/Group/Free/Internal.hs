{-# LANGUAGE PatternSynonyms #-}
{-# language Unsafe #-}
-- |
-- Module       : Data.Group.Free.Internal
-- Copyright    : (c) 2020 Reed Mullanix, Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Reed Mullanix <reedmullanix@gmail.com>,
--                Emily Pillmore <emilypi@cohomolo.gy>
--
-- Stability    : stable
-- Portability  : non-portable
--
-- This module exposes internals of a module "Data.Group.Free".
module Data.Group.Free.Internal
( -- * Free groups
  FreeGroup(..)
  -- ** Free group combinators
, simplify
, interpret
, interpret'
, present
  -- * Free abelian groups
, FreeAbelianGroup(..)
, pattern FreeAbelianGroup
, makeFreeAbelianGroup
, runFreeAbelianGroup
  -- ** Free abelian group combinators
, abfoldMap
, abmap
, abInterpret
, abjoin
, singleton
) where

import Control.Applicative
import Control.Monad

import Data.Bifunctor
import Data.List (foldl')
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
    invert = FreeGroup
      . fmap (either Right Left)
      . reverse
      . runFreeGroup

instance Eq a => GroupOrder (FreeGroup a) where
    -- TODO: It performs simplify each time @order@ is called.
    --   Once "auto-simplify" is implemented, this
    --   call of simplify should be removed.
    order g | simplify g == mempty = Finite 1
            | otherwise           = Infinite

instance Functor FreeGroup where
    fmap f (FreeGroup g) = FreeGroup $ fmap (bimap f f) g

instance Applicative FreeGroup where
    pure a = FreeGroup $ pure $ pure a
    (<*>) = ap

instance Monad FreeGroup where
    return = pure
    (FreeGroup g) >>= f = FreeGroup $ concatMap go g
        where
          go (Left a)  = runFreeGroup $ invert (f a)
          go (Right a) = runFreeGroup $ f a

instance Alternative FreeGroup where
    empty = mempty
    (<|>) = (<>)

-- | /O(n)/ Simplifies a word in a free group.
--
-- === __Examples:__
--
-- >>> simplify $ FreeGroup [Right 'a', Left 'b', Right 'c', Left 'c', Right 'b', Right 'a']
-- FreeGroup {runFreeGroup = [Right 'a',Right 'a']}
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

-- | Present a 'Group' as a 'FreeGroup' modulo relations.
--
present :: Group g => FreeGroup g -> (FreeGroup g -> g) -> g
present = flip ($)
{-# inline present #-}

-- | A representation of a free abelian group over an alphabet @a@.
--
-- The intuition here is group elements correspond with their positive
-- or negative multiplicities, and as such are simplified by construction.
--
-- === __Examples__:
-- 
-- >>> a = singleton 'a'
-- >>> b = singleton 'b'
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
newtype FreeAbelianGroup a =
  MkFreeAbelianGroup (Map a Integer)
    -- ^ Unsafe "raw" constructor, which does no normalization works
    --   'makeFreeAbelianGroup' does. 
  deriving (Eq, Ord)

instance Show a => Show (FreeAbelianGroup a) where
    showsPrec p (MkFreeAbelianGroup g) =
        showParen (p > 0) $ ("FreeAbelianGroup $ " ++) . shows g

-- | /O(n)/ Constructs a 'FreeAbelianGroup' from a finite 'Map' from
--   the set of generators (@a@) to its multiplicities.
makeFreeAbelianGroup :: Ord a => Map a Integer -> FreeAbelianGroup a
makeFreeAbelianGroup = MkFreeAbelianGroup . Map.filter (/= 0)

-- | /O(1)/ Gets a representation of 'FreeAbelianGroup' as
--   'Map'. The returned map contains no records with
--   multiplicity @0@ i.e. @'Map.lookup' a@ on the returned map
--   never returns @Just 0@.
runFreeAbelianGroup :: FreeAbelianGroup a -> Map a Integer
runFreeAbelianGroup (MkFreeAbelianGroup g) = g

pattern FreeAbelianGroup :: Ord a => Map a Integer -> FreeAbelianGroup a
pattern FreeAbelianGroup g <- MkFreeAbelianGroup g where
    FreeAbelianGroup g = makeFreeAbelianGroup g

{-# COMPLETE FreeAbelianGroup #-}

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

-- NOTE: We can't implement Functor/Applicative/Monad here
-- due to the Ord constraint. C'est La Vie!

-- | Given a function from generators to an abelian group @g@,
--   lift that function to a group homomorphism from 'FreeAbelianGroup' to @g@.
--   
--   In other words, it's a function analogus to 'foldMap' for 'Monoid' or
--   'Data.Group.Foldable.goldMap' for @Group@.
abfoldMap :: (Abelian g) => (a -> g) -> FreeAbelianGroup a -> g
abfoldMap f = Map.foldlWithKey' step mempty . runFreeAbelianGroup
  where
    step g a n = g <> pow (f a) n

-- | Functorial 'fmap' for a 'FreeAbelianGroup'.
--
-- === __Examples__:
-- 
-- >>> singleton 'a' <> singleton 'A'
-- FreeAbelianGroup $ fromList [('A',1),('a',1)]
-- >>> import Data.Char (toUpper)
-- >>> abmap toUpper $ singleton 'a' <> singleton 'A'
-- FreeAbelianGroup $ fromList [('A',2)]
abmap :: (Ord b) => (a -> b) -> FreeAbelianGroup a -> FreeAbelianGroup b
abmap f = abfoldMap (singleton . f)

-- | Lift a singular value into a 'FreeAbelianGroup'. Analogous to 'pure'.
--
-- === __Examples__:
-- 
-- >>> singleton "foo"
-- FreeAbelianGroup $ fromList [("foo",1)]
singleton :: a -> FreeAbelianGroup a
singleton a = MkFreeAbelianGroup $ Map.singleton a 1

-- | Monadic 'join' for a 'FreeAbelianGroup'.
--
abjoin :: (Ord a) => FreeAbelianGroup (FreeAbelianGroup a) -> FreeAbelianGroup a
abjoin = abInterpret

-- | Interpret a free group as a word in the underlying group @g@.
--
abInterpret :: (Abelian g) => FreeAbelianGroup g -> g
abInterpret = abfoldMap id
