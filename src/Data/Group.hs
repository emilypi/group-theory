{-# language BangPatterns #-}
{-# language CPP #-}
{-# language DerivingStrategies #-}
{-# language FlexibleInstances #-}
{-# language PatternSynonyms #-}
{-# language Safe #-}
#if MIN_VERSION_base(4,12,0)
{-# language TypeOperators #-}
#endif
{-# language ViewPatterns #-}
-- |
-- Module       : Data.Group
-- Copyright    : (c) 2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>,
--                Reed Mullanix <reedmullanix@gmail.com>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains definitions for 'Group' and 'AbelianGroup',
-- along with the relevant combinators.
--
module Data.Group
( -- * Groups
  Group(..)
  -- * Group combinators
, (><)
  -- ** Conjugation
, conjugate
, unconjugate
, pattern Conjugate
  -- ** Order
, Order(..)
, pattern Infinitary
, pattern Finitary
, order
  -- ** Abelianization
, Abelianizer(..)
, abelianize
, pattern Commutated
, pattern Quotiented
  -- * Abelian groups
, AbelianGroup
) where


import Data.Bool
import Data.Functor.Const
#if __GLASGOW_HASKELL__ > 804
import Data.Functor.Contravariant
#endif
import Data.Functor.Identity
import Data.Semigroup (stimes)
import Data.Int
import Data.Monoid
import Data.Ord
import Data.Proxy
import Data.Ratio
import Data.Word

import Numeric.Natural

#if MIN_VERSION_base(4,12,0)
import GHC.Generics
#endif

import Prelude hiding (negate, exponent)
import qualified Prelude

-- $setup
--
-- >>> import qualified Prelude
-- >>> import Data.Group
-- >>> import Data.Monoid
-- >>> import Data.Semigroup
-- >>> :set -XTypeApplications
-- >>> :set -XFlexibleContexts

infixr 6 ><

-- -------------------------------------------------------------------- --
-- Groups

-- | The typeclass of groups (types with an associative binary operation that
-- has an identity, and all inverses, i.e. a 'Monoid' with all inverses),
-- representing the structural symmetries of a mathematical object.
--
-- Instances should satisfy the following:
--
-- [Right identity] @ x '<>' 'mempty' = x@
-- [Left identity]  @'mempty' '<>' x = x@
-- [Associativity]  @ x '<>' (y '<>' z) = (x '<>' y) '<>' z@
-- [Concatenation]  @ 'mconcat' = 'foldr' ('<>') 'mempty'@
-- [Right inverses] @ x '<>' 'invert' x = 'mempty' @
-- [Left inverses]  @ 'invert' x '<>' x = 'mempty' @
--
-- Some types can be viewed as a group in more than one way,
-- e.g. both addition and multiplication on numbers.
-- In such cases we often define @newtype@s and make those instances
-- of 'Group', e.g. 'Data.Semigroup.Sum' and 'Data.Semigroup.Product'.
-- Often in practice such differences between addition and
-- multiplication-like operations matter (e.g. when defining rings), and
-- so, classes "additive" (the underlying operation is addition-like) and
-- "multiplicative" group classes are provided in vis 'Data.Group.Additive.AdditiveGroup' and
-- 'Data.Group.Multiplicative.MultiplicativeGroup'.
--
-- Categorically, 'Group's may be viewed single-object groupoids.
--
class Monoid a => Group a where
  invert :: a -> a
  invert a = mempty `minus` a
  {-# inline invert #-}

  -- | Similar to 'stimes' from 'Data.Semigroup', but handles
  -- negative numbers by using 'invert'.
  --
  -- === __Examples:__
  --
  -- >>> gtimes 2 (Sum 3)
  -- Sum {getSum = 6}
  -- >>> gtimes (-3) (Sum 3)
  -- Sum {getSum = -9}
  --
  gtimes :: (Integral n) => n -> a -> a
  gtimes n a
    | n == 0 = mempty
    | n > 0 = stimes n a
    | otherwise = stimes (abs n) (invert a)
  {-# inline gtimes #-}

  -- | 'Group' subtraction.
  --
  -- This function denotes principled 'Group' subtraction, where
  -- @a `minus` b@ translates into @a <> (invert b)@. This is because
  -- subtraction as an operator is non-associative, but the operation
  -- described in terms of addition and inversion is.
  --
  minus :: a -> a -> a
  minus a b = a <> invert b
  {-# inline minus #-}
  {-# minimal invert | minus #-}


instance Group () where
  invert = id
  {-# inline invert #-}

instance Group b => Group (a -> b) where
  invert f = invert . f
  {-# inline invert #-}

instance Group a => Group (Dual a) where
  invert (Dual a) = Dual (invert a)
  {-# inline invert #-}

instance Group a => Group (Down a) where
  invert (Down a) = Down (invert a)
  {-# inline invert #-}

instance Group a => Group (Endo a) where
  invert (Endo a) = Endo (invert . a)
  {-# inline invert #-}

#if __GLASGOW_HASKELL__ > 804
instance Group (Equivalence a) where
  invert (Equivalence p) = Equivalence $ \a b -> not (p a b)
  {-# inline invert #-}

instance Group (Comparison a) where
  invert (Comparison p) = Comparison $ \a b -> invert (p a b)
  {-# inline invert #-}

instance Group (Predicate a) where
  invert (Predicate p) = Predicate $ \a -> not (p a)
  {-# inline invert #-}

instance Group a => Group (Op a b) where
  invert (Op f) = Op $ invert . f
  {-# inline invert #-}
#endif

instance Group Any where
  invert (Any b) = Any $ bool True False b
  {-# inline invert #-}

instance Group All where
  invert (All b) = All $ bool True False b
  {-# inline invert #-}

instance Group (Sum Integer) where
  invert = Prelude.negate
  {-# inline invert #-}

instance Group (Sum Rational) where
  invert = Prelude.negate
  {-# inline invert #-}

instance Group (Sum Int) where
  invert = Prelude.negate
  {-# inline invert #-}

instance Group (Sum Int8) where
  invert = Prelude.negate
  {-# inline invert #-}

instance Group (Sum Int16) where
  invert = Prelude.negate
  {-# inline invert #-}

instance Group (Sum Int32) where
  invert = Prelude.negate
  {-# inline invert #-}

instance Group (Sum Int64) where
  invert = Prelude.negate
  {-# inline invert #-}

instance Group (Sum Word) where
  invert = Prelude.negate
  {-# inline invert #-}

instance Group (Sum Word8) where
  invert = Prelude.negate
  {-# inline invert #-}

instance Group (Sum Word16) where
  invert = Prelude.negate
  {-# inline invert #-}

instance Group (Sum Word32) where
  invert = Prelude.negate
  {-# inline invert #-}

instance Group (Sum Word64) where
  invert = Prelude.negate
  {-# inline invert #-}

instance Group (Sum (Ratio Int)) where
  invert = Sum . Prelude.negate . getSum
  {-# inline invert #-}

instance Group (Sum (Ratio Int8)) where
  invert = Sum . Prelude.negate . getSum
  {-# inline invert #-}

instance Group (Sum (Ratio Int16)) where
  invert = Sum . Prelude.negate . getSum
  {-# inline invert #-}

instance Group (Sum (Ratio Int32)) where
  invert = Sum . Prelude.negate . getSum
  {-# inline invert #-}

instance Group (Sum (Ratio Int64)) where
  invert = Sum . Prelude.negate . getSum
  {-# inline invert #-}

instance Group (Sum (Ratio Word)) where
  invert = Sum . Prelude.negate . getSum
  {-# inline invert #-}

instance Group (Sum (Ratio Word8)) where
  invert = Sum . Prelude.negate . getSum
  {-# inline invert #-}

instance Group (Sum (Ratio Word16)) where
  invert = Sum . Prelude.negate . getSum
  {-# inline invert #-}

instance Group (Sum (Ratio Word32)) where
  invert = Sum . Prelude.negate . getSum
  {-# inline invert #-}

instance Group (Sum (Ratio Word64)) where
  invert = Sum . Prelude.negate . getSum
  {-# inline invert #-}

instance Group (Product Rational) where
  invert = Product . Prelude.recip . getProduct
  {-# inline invert #-}

instance Group (Product (Ratio Natural)) where
  invert = Product . Prelude.recip . getProduct
  {-# inline invert #-}

instance Group (Product (Ratio Int)) where
  invert = Product . Prelude.recip . getProduct
  {-# inline invert #-}

instance Group (Product (Ratio Int8)) where
  invert = Product . Prelude.recip . getProduct
  {-# inline invert #-}

instance Group (Product (Ratio Int16)) where
  invert = Product . Prelude.recip . getProduct
  {-# inline invert #-}

instance Group (Product (Ratio Int32)) where
  invert = Product . Prelude.recip . getProduct
  {-# inline invert #-}

instance Group (Product (Ratio Int64)) where
  invert = Product . Prelude.recip . getProduct
  {-# inline invert #-}

instance Group (Product (Ratio Word)) where
  invert = Product . Prelude.recip . getProduct
  {-# inline invert #-}

instance Group (Product (Ratio Word8)) where
  invert = Product . Prelude.recip . getProduct
  {-# inline invert #-}

instance Group (Product (Ratio Word16)) where
  invert = Product . Prelude.recip . getProduct
  {-# inline invert #-}

instance Group (Product (Ratio Word32)) where
  invert = Product . Prelude.recip . getProduct
  {-# inline invert #-}

instance Group (Product (Ratio Word64)) where
  invert = Product . Prelude.recip . getProduct
  {-# inline invert #-}

instance Group a => Group (Const a b) where
  invert = Const . invert . getConst
  {-# inline invert #-}

instance Group a => Group (Identity a) where
  invert = Identity . invert . runIdentity
  {-# inline invert #-}

instance Group Ordering where
  invert LT = GT
  invert EQ = EQ
  invert GT = LT
  {-# inline invert #-}

instance (Group a, Group b) => Group (a,b) where
  invert ~(a,b) = (invert a, invert b)
  {-# inline invert #-}

instance Group a => Group (Proxy a) where
  invert _ = Proxy

instance (Group a, Group b, Group c) => Group (a,b,c) where
  invert ~(a,b,c) = (invert a, invert b, invert c)
  {-# inline invert #-}

instance (Group a, Group b, Group c, Group d) => Group (a,b,c,d) where
  invert ~(a,b,c,d) = (invert a, invert b, invert c, invert d)
  {-# inline invert #-}

instance (Group a, Group b, Group c, Group d, Group e) => Group (a,b,c,d,e) where
  invert ~(a,b,c,d,e) = (invert a, invert b, invert c, invert d, invert e)
  {-# inline invert #-}

#if MIN_VERSION_base(4,12,0)
instance (Group (f a), Group (g a)) => Group ((f :*: g) a) where
  invert (f :*: g) = invert f :*: invert g

instance Group (f (g a)) => Group ((f :.: g) a) where
  invert (Comp1 fg) = invert (Comp1 fg)
#endif

-- -------------------------------------------------------------------- --
-- Group combinators

-- | Apply @('<>')@, commuting its arguments. When the group is abelian,
-- @a <> b@ is identically @b <> a@.
--
(><) :: Group a => a -> a -> a
a >< b = b <> a
{-# inline (><) #-}

-- | Conjugate an element of a group by another element.
-- When the group is abelian, conjugation is the identity.
--
-- Symbolically, this is \( a \mapsto gag^{-1} \).
--
-- === __Examples__:
--
-- >>> let x = Sum (3 :: Int)
-- >>> conjugate x x
-- Sum {getSum = 3}
--
-- >>> let x = All True
-- >>> conjugate (All False) x
-- All {getAll = False}
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

-- -------------------------------------------------------------------- --
-- Group order

-- | The order of a group element.
--
-- The order of a group element can either be infinite,
-- as in the case of @All False@, or finite, as in the
-- case of @All True@.
--
data Order = Infinite | Finite !Natural
  deriving (Eq, Show)

-- | Unidirectional pattern synonym for the infinite order of a
-- group element.
--
pattern Infinitary :: (Eq g, Group g) => g
pattern Infinitary <- (order -> Infinite)

-- | Unidirectional pattern synonym for the finite order of a
-- group element.
--
pattern Finitary :: (Eq g, Group g) => Natural -> g
pattern Finitary n <- (order -> Finite n)

-- | Calculate the exponent of a particular element in a group.
--
-- __Warning:__ If 'order' expects a 'FiniteGroup', this is gauranteed
-- to terminate. However, this is not true of groups in general. This will
-- spin forever if you give it something like non-zero @Sum Integer@.
--
-- === __Examples__:
--
-- >>> order @(Sum Word8) 3
-- Finite 255
--
-- >>> order (Any False)
-- Finite 1
--
-- >>> order (All False)
-- Infinite
--
order :: (Eq g, Group g) => g -> Order
order a = go 0 a where
  go !n g
    -- guard against ().
    | g == mempty, n > 0 = Finite n
    -- guard against infinite cases like @All False@.
    | g == a, n > 0 = Infinite
    | otherwise = go (succ n) (g <> a)
{-# inline order #-}

-- -------------------------------------------------------------------- --
-- Abelianization

-- | Quotient a pair of group elements by their commutator.
--
-- The quotient \( G / [G,G] \) forms an abelian group, and 'Abelianizer'
-- forms a functor from the category of groups to the category of Abelian groups.
-- This functor is, in fact, a monad in \( Grp \) when this functor
-- is composed with the forgetful functor \( Ab \rightarrow Grp \)
-- "forgetting" commutativity.
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
  -- Normally we'd say 'Quot' but these are the same.
  mempty = Commuted mempty

instance (Eq g, Group g) => Group (Abelianizer g) where
  invert Quot = Quot
  invert (Commuted a)
    | a == mempty = Quot
    | otherwise = Commuted (invert a)

-- | Quotient a pair of group elements by their commutator.
--
-- Ranging over the entire group, this operation constructs
-- the quotient of the group by its commutator sub-group
-- \( G / [G,G] \), where \( [G,G] \) maps to the kerel of
-- 'abelianize'.
--
abelianize :: (Eq g, Group g) => g -> g -> Abelianizer g
abelianize g g'
  | x == mempty = Quot
  | otherwise = Commuted x
  where
    x = g <> g' <> invert g <> invert g'

-- | A unidirectional pattern synonym for elements of a group
-- modulo commutators which are __not__ the identity.
--
pattern Commutated :: (Eq g, Group g) => g -> (g,g)
pattern Commutated x <- (uncurry abelianize -> Commuted x)

-- | A unidirectional pattern synonym for elements of a group
-- modulo commutators which are the identity.
--
pattern Quotiented :: (Eq g, Group g) => (g,g)
pattern Quotiented <- (uncurry abelianize -> Quot)

-- -------------------------------------------------------------------- --
-- Abelian (commutative) groups

-- | Commutative 'Group's.
--
-- Instances of 'AbelianGroup' satisfy the following laws:
--
-- [Commutativity] @x <> y = y <> x@
--
class Group a => AbelianGroup a
instance AbelianGroup ()
instance AbelianGroup b => AbelianGroup (a -> b)
instance AbelianGroup a => AbelianGroup (Dual a)
instance AbelianGroup Any
instance AbelianGroup All
instance AbelianGroup (Sum Integer)
instance AbelianGroup (Sum Int)
instance AbelianGroup (Sum Int8)
instance AbelianGroup (Sum Int16)
instance AbelianGroup (Sum Int32)
instance AbelianGroup (Sum Int64)
instance AbelianGroup (Sum Word)
instance AbelianGroup (Sum Word8)
instance AbelianGroup (Sum Word16)
instance AbelianGroup (Sum Word32)
instance AbelianGroup (Sum Word64)
instance AbelianGroup (Sum (Ratio Integer))
instance AbelianGroup (Sum (Ratio Int))
instance AbelianGroup (Sum (Ratio Int8))
instance AbelianGroup (Sum (Ratio Int16))
instance AbelianGroup (Sum (Ratio Int32))
instance AbelianGroup (Sum (Ratio Int64))
instance AbelianGroup (Sum (Ratio Word))
instance AbelianGroup (Sum (Ratio Word8))
instance AbelianGroup (Sum (Ratio Word16))
instance AbelianGroup (Sum (Ratio Word32))
instance AbelianGroup (Sum (Ratio Word64))
instance AbelianGroup (Product (Ratio Integer))
instance AbelianGroup (Product (Ratio Int))
instance AbelianGroup (Product (Ratio Int8))
instance AbelianGroup (Product (Ratio Int16))
instance AbelianGroup (Product (Ratio Int32))
instance AbelianGroup (Product (Ratio Int64))
instance AbelianGroup (Product (Ratio Word))
instance AbelianGroup (Product (Ratio Word8))
instance AbelianGroup (Product (Ratio Word16))
instance AbelianGroup (Product (Ratio Word32))
instance AbelianGroup (Product (Ratio Word64))
instance AbelianGroup (Product (Ratio Natural))
instance AbelianGroup a => AbelianGroup (Const a b)
instance AbelianGroup a => AbelianGroup (Identity a)
instance AbelianGroup a => AbelianGroup (Proxy a)
instance AbelianGroup Ordering
instance (AbelianGroup a, AbelianGroup b) => AbelianGroup (a,b)
instance (AbelianGroup a, AbelianGroup b, AbelianGroup c) => AbelianGroup (a,b,c)
instance (AbelianGroup a, AbelianGroup b, AbelianGroup c, AbelianGroup d) => AbelianGroup (a,b,c,d)
instance (AbelianGroup a, AbelianGroup b, AbelianGroup c, AbelianGroup d, AbelianGroup e) => AbelianGroup (a,b,c,d,e)
instance AbelianGroup a => AbelianGroup (Down a)
instance AbelianGroup a => AbelianGroup (Endo a)
#if MIN_VERSION_base(4,12,0)
instance (AbelianGroup (f a), AbelianGroup (g a)) => AbelianGroup ((f :*: g) a)
instance AbelianGroup (f (g a)) => AbelianGroup ((f :.: g) a)
#endif

#if __GLASGOW_HASKELL__ > 804
instance AbelianGroup (Equivalence a)
instance AbelianGroup (Comparison a)
instance AbelianGroup (Predicate a)
instance AbelianGroup a => AbelianGroup (Op a b)
#endif

instance (Eq a, AbelianGroup a) => AbelianGroup (Abelianizer a)
