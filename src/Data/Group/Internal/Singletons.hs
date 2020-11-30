{-# language AllowAmbiguousTypes #-}
{-# language DerivingStrategies #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeInType #-}
{-# language ViewPatterns #-}
-- |
-- Module       : Data.Group.Internal.Singletons
-- Copyright    : (c) 2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
module Data.Group.Internal.Singletons
( SingKind(..)
, SingI(..)
, SNat(..)
, pattern Nat
, pattern S
, pattern Z
, safePred
) where


import Data.Kind
import Data.Type.Equality

import GHC.TypeNats

import Numeric.Natural

import Unsafe.Coerce

-- -------------------------------------------------------------------- --
-- Singletons

class SingKind k where
  type Sing :: k -> Type
  fromSing :: Sing (a :: k) -> k

class SingI (a :: k) where
  sing :: Sing a

-- -------------------------------------------------------------------- --
-- Lowering nats

newtype SNat (n :: Nat) = UnsafeSNat { snatVal :: Natural }
  deriving newtype (Eq, Show)

instance SingKind Nat where
  type Sing = SNat
  fromSing = unsafeCoerce

instance TestEquality SNat where
  testEquality i j
    | snatVal i == snatVal j = Just (unsafeCoerce Refl)
    | otherwise = Nothing

toNat :: Natural -> Nat
toNat = unsafeCoerce

fromNat :: Nat -> Natural
fromNat = unsafeCoerce

pattern Nat :: Natural -> Nat
pattern Nat n <- (fromNat -> n) where
  Nat n = toNat n

{-# complete Nat #-}

pattern Z :: Nat
pattern Z = Nat 0

safePred :: Natural -> Maybe Natural
safePred 0 = Nothing
safePred n = Just (n-1)

pattern S :: Nat -> Nat
pattern S n <- (safePred . fromNat -> Just (toNat -> n)) where
  S n = toNat (fromNat n + 1)

{-# complete Z, S #-}
