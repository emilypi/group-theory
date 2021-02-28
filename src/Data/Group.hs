{-# language CPP #-}
{-# language DerivingStrategies #-}
{-# language FlexibleInstances #-}
{-# language PackageImports #-}
{-# language PatternSynonyms #-}
{-# language Safe #-}
#if MIN_VERSION_base(4,12,0)
{-# language TypeOperators #-}
#endif
{-# language ViewPatterns #-}
-- |
-- Module       : Data.Group
-- Copyright    : (c) 2020-2021 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>,
--                Reed Mullanix <reedmullanix@gmail.com>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains definitions for 'Group' and 'Abelian',
-- along with the relevant combinators.
--
module Data.Group
( -- * Groups
  -- $groups
  G.Group(..)
  -- * Group combinators
, minus
, gtimes
, (><)
  -- ** Conjugation
, conjugate
, unconjugate
, pattern Conjugate
  -- ** Elements
, pattern Inverse
, pattern IdentityElem
  -- ** Abelianization
, Abelianizer(..)
, abelianize
, commutate
, pattern Abelianized
, pattern Quotiented
  -- * Abelian groups
  -- $abelian
, G.Abelian
) where


import Data.Bool
import "groups" Data.Group as G
import Data.Monoid

import Prelude hiding (negate, exponent)

-- $setup
--
-- >>> :set -XPackageImports
-- >>> import qualified Prelude
-- >>> import "group-theory" Data.Group
-- >>> import Data.Monoid
-- >>> import Data.Semigroup
-- >>> import Data.Word
-- >>> :set -XTypeApplications
-- >>> :set -XFlexibleContexts

infixr 6 ><

-- -------------------------------------------------------------------- --
-- Group combinators

{- $groups

The typeclass of groups (types with an associative binary operation that
has an identity, and all inverses, i.e. a 'Monoid' with all inverses),
representing the structural symmetries of a mathematical object.

Instances should satisfy the following:

[Right identity] @ x '<>' 'mempty' = x@
[Left identity]  @'mempty' '<>' x = x@
[Associativity]  @ x '<>' (y '<>' z) = (x '<>' y) '<>' z@
[Concatenation]  @ 'mconcat' = 'foldr' ('<>') 'mempty'@
[Right inverses] @ x '<>' 'invert' x = 'mempty' @
[Left inverses]  @ 'invert' x '<>' x = 'mempty' @

Some types can be viewed as a group in more than one way,
e.g. both addition and multiplication on numbers.
In such cases we often define @newtype@s and make those instances
of 'Group', e.g. 'Data.Semigroup.Sum' and 'Data.Semigroup.Product'.
Often in practice such differences between addition and
multiplication-like operations matter (e.g. when defining rings), and
so, classes "additive" (the underlying operation is addition-like) and
"multiplicative" group classes are provided in vis 'Data.Group.Additive.AdditiveGroup' and
'Data.Group.Multiplicative.MultiplicativeGroup'.

Categorically, 'Group's may be viewed single-object groupoids.
-}

-- | An alias to 'pow'.
--
-- Similar to 'Data.Semigroup.stimes' from 'Data.Semigroup', but handles
-- negative powers by using 'invert' appropriately.
--
-- === __Examples:__
--
-- >>> gtimes 2 (Sum 3)
-- Sum {getSum = 6}
-- >>> gtimes (-3) (Sum 3)
-- Sum {getSum = -9}
--
gtimes :: (Group a, Integral n) => n -> a -> a
gtimes = flip pow
{-# inline gtimes #-}

-- | 'Group' subtraction.
--
-- This function denotes principled 'Group' subtraction, where
-- @a `minus` b@ translates into @a <> invert b@. This is because
-- subtraction as an operator is non-associative, but the operation
-- described in terms of addition and inversion is.
--
minus :: Group a => a -> a -> a
minus a b = a <> invert b
{-# inline minus #-}

-- | Apply @('<>')@, commuting its arguments. When the group is abelian,
-- @a <> b@ is identically @b <> a@.
--
(><) :: Group a => a -> a -> a
a >< b = b <> a
{-# inline (><) #-}

-- -------------------------------------------------------------------- --
-- Group conjugation

-- | Conjugate an element of a group by another element.
-- When the group is abelian, conjugation is the identity.
--
-- Symbolically, this is \( (g,a) \mapsto gag^{-1} \).
--
-- === __Examples__:
--
-- >>> let x = Sum (3 :: Int)
-- >>> conjugate x x
-- Sum {getSum = 3}
--
conjugate :: Group a => a -> a -> a
conjugate g a = (g <> a) `minus` g
{-# inline conjugate #-}

-- | Apply an inverse conjugate to a conjugated element.
--
-- @
-- unconjugate . conjugate = id
-- conjugate . unconjugate = id
-- @
--
-- === __Examples__:
--
-- >>> let x = Sum (3 :: Int)
-- >>> unconjugate x (conjugate x x)
-- Sum {getSum = 3}
--
unconjugate :: Group a => a -> a -> a
unconjugate g a = invert g <> a <> g

-- | Bidirectional pattern for conjugation by a group element
--
-- __Note:__ When the underlying 'Group' is abelian, this
-- pattern is the identity.
--
pattern Conjugate :: Group a => (a,a) -> (a,a)
pattern Conjugate t <- (\(g,a) -> (g, conjugate g a) -> t) where
  Conjugate (g,a) = (g, unconjugate g a)
{-# complete Conjugate #-}

-- | Bidirectional pattern for inverse elements.
pattern Inverse :: (Group g) => g -> g
pattern Inverse t <- (invert -> t) where
    Inverse g = invert g

-- | Bidirectional pattern for the identity element.
pattern IdentityElem :: (Eq m, Monoid m) => m
pattern IdentityElem <- ((== mempty) -> True) where
  IdentityElem = mempty

-- -------------------------------------------------------------------- --
-- Abelianization

-- | Quotient a pair of group elements by their commutator.
--
-- The of the quotient \( G / [G,G] \) forms an abelian group, and 'Abelianizer'
-- forms a functor from the category of groups to the category of Abelian groups.
-- This functor is left adjoint to the inclusion functor \( Ab \rightarrow Grp \),
-- forming a monad in \( Grp \).
--
data Abelianizer a = Quot | Commuted a
  deriving stock (Eq, Show)

instance Functor Abelianizer where
  fmap _ Quot = Quot
  fmap f (Commuted a) = Commuted (f a)

instance Applicative Abelianizer where
  pure = Commuted

  Quot <*> _ = Quot
  _ <*> Quot = Quot
  Commuted f <*> Commuted a = Commuted (f a)

instance Monad Abelianizer where
  return = pure
  (>>) = (*>)

  Quot >>= _ = Quot
  Commuted a >>= f = f a

instance Foldable Abelianizer where
  foldMap _ Quot = mempty
  foldMap f (Commuted a) = f a

instance Traversable Abelianizer where
  traverse _ Quot = pure Quot
  traverse f (Commuted a) = Commuted <$> f a

instance Semigroup g => Semigroup (Abelianizer g) where
  Quot <> t = t
  t <> Quot = t
  Commuted a <> Commuted b = Commuted (a <> b)

instance Monoid g => Monoid (Abelianizer g) where
  mempty = Quot

instance (Eq g, Group g) => Group (Abelianizer g) where
  invert Quot = Quot
  invert (Commuted IdentityElem) = Quot
  invert (Commuted a) = Commuted (invert a)

-- | Take the commutator of two elements of a group.
--
commutate :: Group g => g -> g -> g
commutate g g' = g <> g' <> invert g <> invert g'
{-# inline commutate #-}

-- | Quotient a pair of group elements by their commutator.
--
-- Ranging over the entire group, this operation constructs
-- the quotient of the group by its commutator sub-group
-- \( G / [G,G] \).
--
abelianize :: (Eq g, Group g) => g -> g -> Abelianizer g
abelianize g g'
  | x == mempty = Quot
  | otherwise = Commuted x
  where
    x = commutate g g'
{-# inline abelianize #-}

-- | A unidirectional pattern synonym for elements of a group
-- modulo commutators which are __not__ the identity.
--
pattern Abelianized :: (Eq g, Group g) => g -> (g,g)
pattern Abelianized x <- (uncurry abelianize -> Commuted x)

-- | A unidirectional pattern synonym for elements of a group
-- modulo commutators which are the identity.
--
pattern Quotiented :: (Eq g, Group g) => (g,g)
pattern Quotiented <- (uncurry abelianize -> Quot)

-- -------------------------------------------------------------------- --
-- Abelian (commutative) groups

{- $abelian
Commutative 'Group's.

Instances of 'Abelian' satisfy the following laws:

[Commutativity] @x <> y = y <> x@
-}
