{-# language DefaultSignatures #-}
{-# language Safe #-}
-- |
-- Module       : Control.Applicative.Cancellative
-- Copyright    : (c) 2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>,
--                Reed Mullanix <reedmullanix@gmail.com>
--
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains definitions for 'Cancellative' functors
-- along with the relevant combinators.
--
module Control.Applicative.Cancellative
( -- * Cancellative
  Cancellative(..)
  -- ** Cancellative combinators
, cancel1
, annihilate
) where


import Control.Applicative
import Data.Group
import Data.Group.Free
import Data.Group.Free.Church
import Data.Proxy

-- $setup
--
-- >>> :set -XPackageImports
-- >>> import Prelude
-- >>> import "group-theory" Data.Group
-- >>> import Data.Monoid
-- >>> import Data.Semigroup
-- >>> import Data.Word
-- >>> import Data.Group.Free
-- >>> import Data.Group.Foldable
-- >>> :set -XTypeApplications
-- >>> :set -XFlexibleContexts

-- -------------------------------------------------------------------- --
-- Cancellative functors

-- | A group on 'Applicative' functors.
--
-- 'Cancellative' functors have the following laws in addition to those of
-- 'Alternative':
--
-- [Left Cancellation] @ 'cancel' a '<|>' a = 'empty' @
-- [Rigth Cancellation] @ a '<|>' 'cancel' a = 'empty' @
--
-- This is analogous to a group operation on applicative functors,
-- in the sense that 'Alternative' forms a monoid. A straight-
-- forward implementation exists whenever @f a@ forms a 'Group'
-- for all @a@, in which case, @cancel == invert@.
--
class Alternative f => Cancellative f where
  -- | Invert (or 'cancel') a 'Cancellative' functor, such that, if the
  -- functor is also a 'Data.Group.Foldable.GroupFoldable', then @'Data.Group.Foldable.gold' '.' 'cancel'@
  -- amounts to evaluating the inverse of a word in the functor.
  --
  -- === __Examples:__
  --
  -- >>> let x = FreeGroup [Left (Sum (2 :: Word8)), Right (Sum 3)]
  -- >>> cancel x
  -- FreeGroup {runFreeGroup = [Left (Sum {getSum = 3}),Right (Sum {getSum = 2})]}
  --
  cancel :: f a -> f a
  default cancel :: Group (f a) => f a -> f a
  cancel = invert

instance Cancellative FG
instance Cancellative FA
instance Cancellative FreeGroup
instance Cancellative Proxy where
  cancel _ = Proxy

-- -------------------------------------------------------------------- --
-- Cancellative functor combinators

-- | Cancel a single element in a 'Cancellative' functor.
--
-- === __Examples:__
--
-- >>> let x = FreeGroup [Left (Sum (2 :: Word8)), Right (Sum 3)]
-- >>> gold x
-- Sum {getSum = 1}
-- >>> gold $ cancel1 (Sum 1) x
-- Sum {getSum = 0}
--
cancel1 :: (Group a, Cancellative f) => a -> f a -> f a
cancel1 a f = cancel (pure a) <|> f

-- | Annihilate a 'Traversable'\'s worth of elements in a 'Cancellative'
-- functor.
--
annihilate :: (Cancellative f, Traversable t) => (a -> f a) -> t a -> f (t a)
annihilate f = traverse (cancel . f)
