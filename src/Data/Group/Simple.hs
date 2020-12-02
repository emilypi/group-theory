{-# language FlexibleInstances #-}
{-# language Safe #-}
-- |
-- Module       : Data.Group.Simple
-- Copyright    : (c) 2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains definitions for 'SimpleGroup's
-- along with the relevant combinators.
--
module Data.Group.Simple
( -- * Simple groups
  SimpleGroup
) where


import Data.Functor.Const
import Data.Functor.Identity
import Data.Group
import Data.Int
import Data.Monoid
import Data.Proxy
import Data.Ratio
import Data.Word

import Numeric.Natural

class Group g => SimpleGroup g

instance SimpleGroup ()
instance SimpleGroup a => SimpleGroup (Dual a)
instance SimpleGroup All
instance SimpleGroup (Sum Integer)
instance SimpleGroup (Sum Rational)
instance SimpleGroup (Sum Int)
instance SimpleGroup (Sum Int8)
instance SimpleGroup (Sum Int16)
instance SimpleGroup (Sum Int32)
instance SimpleGroup (Sum Int64)
instance SimpleGroup (Sum Word)
instance SimpleGroup (Sum Word8)
instance SimpleGroup (Sum Word16)
instance SimpleGroup (Sum Word32)
instance SimpleGroup (Sum Word64)
instance SimpleGroup (Product (Ratio Integer))
instance SimpleGroup (Product (Ratio Natural))
instance SimpleGroup (Product (Ratio Int))
instance SimpleGroup (Product (Ratio Int8))
instance SimpleGroup (Product (Ratio Int16))
instance SimpleGroup (Product (Ratio Int32))
instance SimpleGroup (Product (Ratio Int64))
instance SimpleGroup (Product (Ratio Word))
instance SimpleGroup (Product (Ratio Word8))
instance SimpleGroup (Product (Ratio Word16))
instance SimpleGroup (Product (Ratio Word32))
instance SimpleGroup (Product (Ratio Word64))
instance SimpleGroup a => SimpleGroup (Const a b)
instance SimpleGroup a => SimpleGroup (Identity a)
instance SimpleGroup a => SimpleGroup (Proxy a)
instance SimpleGroup Ordering
instance (SimpleGroup a, SimpleGroup b) => SimpleGroup (a,b)
instance (SimpleGroup a, SimpleGroup b, SimpleGroup c) => SimpleGroup (a,b,c)
instance (SimpleGroup a, SimpleGroup b, SimpleGroup c, SimpleGroup d) => SimpleGroup (a,b,c,d)
instance (SimpleGroup a, SimpleGroup b, SimpleGroup c, SimpleGroup d, SimpleGroup e) => SimpleGroup (a,b,c,d,e)
