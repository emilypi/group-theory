{-# language AllowAmbiguousTypes #-}
{-# language DerivingStrategies #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language RankNTypes #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeInType #-}
{-# language ViewPatterns #-}
-- |
-- Module       : Data.Group.Internal.Exponent
-- Copyright    : (c) 2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
module Data.Group.Internal.Exponent
(
) where


import Data.Group.Internal.Singletons
import Data.Kind
import Data.Proxy
import Data.Reflection
import Data.Type.Equality

import GHC.TypeNats

import Numeric.Natural

import Unsafe.Coerce


data Exponent = Infinite | Finite !Natural deriving (Eq, Show)


data SExp' (e :: Exponent) where
  SI :: SExp' 'Infinite
  SF :: SExp' ('Finite n)

instance Reifies 'Infinite (SExp' 'Infinite) where
  reflect _ = SI

instance Reifies ('Finite n) (SExp' ('Finite n)) where
  reflect _ = SF
