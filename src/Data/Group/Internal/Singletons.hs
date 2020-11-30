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
(
) where


import Data.Kind
import Data.Type.Equality

import GHC.TypeNats

import Numeric.Natural

import Unsafe.Coerce
