{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
module Data.Group.Quaternion where



import Data.Ix
import Data.Data
import Data.Group
import Data.Group.Finite

import GHC.Generics


data Quaternion = E | E' | I | I' | J | J' | K | K'
  deriving
    ( Eq, Ord, Enum, Read, Show, Bounded, Ix
    , Data, Typeable
    , Generic
    )

instance Semigroup Quaternion where
  E <> q = q
  p <> E = p
  E' <> t = negativeOf t
  t <> E' = negativeOf t
  I <> J = K
  I <> K = J'
  I <> I' = E
  I <> J' = K'
  I <> K' = J
  J <> I = K'
  J <> K = I
  J <> J' = E
  J <> I' = K
  J <> K' = I'
  K <> I = J
  K <> J = I'
  K <> K' = E
  K <> I' = J'
  K <> J' = I

  I' <> J = K
  I' <> K = J'
  I' <> I' = E
  I' <> J' = K'
  I' <> K' = J
  J' <> I = K'
  J' <> K = I
  J' <> J' = E
  J' <> I' = K
  J' <> K' = I'
  K' <> I = J
  K' <> J = I'
  K' <> K' = E
  K' <> I' = J'
  K' <> J' = I

instance Monoid Quaternion where
  mempty = E


instance Group Quaternion where
  invert = negativeOf

negativeOf :: Quaternion -> Quaternion
negativeOf = \case
  E -> E'
  I -> I'
  J -> J'
  K -> K'
  E' -> E
  I' -> I
  J' -> J
  K' -> K

instance FiniteGroup Quaternion
