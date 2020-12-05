{-# language DefaultSignatures #-}
{-# language Safe #-}
-- |
-- Module       : Control.Applicative.Cancelative
-- Copyright    : (c) 2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>,
--                Reed Mullanix <reedmullanix@gmail.com>
--
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains definitions for 'Cancelative' functors
-- along with the relevant combinators.
--
module Control.Applicative.Cancelative
( -- * Cancelative
  Cancelative(..)
  -- ** Cancelative combinators
, cancel1
, annihalate
) where


import Control.Applicative
import Data.Group
import Data.Group.Free
import Data.Group.Free.Church
import Data.Proxy

-- $setup
--
-- >>> import qualified Prelude
-- >>> import Data.Group
-- >>> import Data.Monoid
-- >>> import Data.Semigroup
-- >>> import Data.Word
-- >>> import Data.Group.Free
-- >>> import Data.Group.Foldable
-- >>> :set -XTypeApplications
-- >>> :set -XFlexibleContexts

-- -------------------------------------------------------------------- --
-- Cancelative functors

-- | A group on 'Applicative' functors.
--
-- 'Cancelative' functors have the following laws:
--
-- [Left Cancelation] @ 'cancel' a '<|>' a = 'empty' @
-- [Rigth Cancelation] @ a '<|>' 'cancel' a = 'empty' @
--
-- This is analogous to a group operation on applicative functors,
-- in the sense that 'Alternative' forms a monoid. A straight-
-- forward implementation exists whenever @f a@ forms a 'Group'
-- for all @a@, in which case, @cancel == invert@.
--
class Alternative f => Cancelative f where
  -- | Invert (or 'cancel') a 'Cancelative' functor, such that, if the
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
  {-# minimal cancel #-}

instance Cancelative FG where
  cancel = invert

instance Cancelative FA where
  cancel = invert

instance Cancelative FreeGroup where
  cancel = invert
    . FreeGroup
    . reverse
    . runFreeGroup

instance Cancelative Proxy where
  cancel _ = Proxy

-- -------------------------------------------------------------------- --
-- Cancelative functor combinators

-- | Cancel a single element in a 'Cancelative' functor.
--
-- === __Examples:__
--
-- >>> let x = FreeGroup [Left (Sum (2 :: Word8)), Right (Sum 3)]
-- >>> gold x
-- Sum {getSum = 1}
-- >>> gold $ cancel1 (Sum 1) x
-- Sum {getSum = 0}
--
cancel1 :: (Group a, Cancelative f) => a -> f a -> f a
cancel1 a f = cancel (pure a) <|> f

-- | Annihalate a 'Traversable''s worth of elements in a 'Cancelative'
-- functor.
--
annihalate :: (Cancelative f, Traversable t) => (a -> f a) -> t a -> f (t a)
annihalate f = traverse (cancel . f)
