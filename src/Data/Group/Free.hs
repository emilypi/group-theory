{-# LANGUAGE PatternSynonyms #-}
{-# language Trustworthy #-}
-- |
-- Module       : Data.Group.Free
-- Copyright    : (c) 2020 Reed Mullanix, Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Reed Mullanix <reedmullanix@gmail.com>,
--                Emily Pillmore <emilypi@cohomolo.gy>
--
-- Stability    : stable
-- Portability  : non-portable
--
-- This module provides definitions for 'FreeGroup's and 'FreeAbelianGroup's,
-- along with useful combinators.
--
module Data.Group.Free
( -- * Free groups
  FreeGroup(..)
  -- ** Free group combinators
, simplify
, interpret
, interpret'
, present
  -- * Free abelian groups
, FreeAbelianGroup()
, pattern FreeAbelianGroup
, makeFreeAbelianGroup
, runFreeAbelianGroup
  -- ** Free abelian group combinators
, abfoldMap
, abmap
, abjoin
, singleton
, abInterpret
) where

import Data.Group.Free.Internal
