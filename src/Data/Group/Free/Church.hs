{-# language RankNTypes #-}
module Data.Group.Free.Church where

import Control.Applicative
import Control.Monad

import Data.Foldable
import Data.Group
import Data.Group.Free
import qualified Data.Map.Strict as Map

-- FIXME: Good name pls
newtype FG a = FG { unFG :: forall g. (Group g) => (a -> g) -> g }

instance Semigroup (FG a) where
    (FG g) <> (FG g') = FG $ \k -> g k <> g' k

instance Monoid (FG a) where
    mempty = FG $ \_ -> mempty

instance Group (FG a) where
    invert (FG g) = FG $ \k -> invert $ g k

instance Functor FG where
    fmap f (FG fa) = FG $ \k -> fa (\a -> k (f a))

instance Applicative FG where
    pure a = FG $ \k -> k a
    (<*>) = ap

instance Monad FG where
    return = pure
    (FG fg) >>= f = FG $ \k -> fg (\a -> (unFG $ f a) k)

instance Alternative FG where
    empty = mempty
    (<|>) = (<>)

interpretFG :: Group g => FG g -> g
interpretFG (FG fg) = fg id

reifyFG :: FG a -> FreeGroup a
reifyFG fg = interpretFG $ fmap pure fg

reflectFG :: FreeGroup a -> FG a
reflectFG (FreeGroup fg) = FG $ \k -> foldMap k fg

    
-- present :: (Group g) => FG g -> (FreeGroup g -> g) -> g
-- present (FG fg) p = _ $ fmap p fg

----------------------------------------
-- Free Abelian Groups

-- FIXME: Good name pls
-- :)
newtype FA a = FA { unFA :: forall g. (Group g) => (a -> Int -> g) -> g }

instance Semigroup (FA a) where
    (FA g) <> (FA g') = FA $ \k -> g k <> g' k

instance Monoid (FA a) where
    mempty = FA $ \_ -> mempty

instance Group (FA a) where
    invert (FA g) = FA $ \k -> invert $ g k

-- Contravariant pls!!
instance Functor FA where
    fmap f (FA fa) = FA $ \k -> fa (\a n -> k (f a) n)

instance Applicative FA where
    pure a = FA $ \k -> k a 1
    (<*>) = ap

instance Monad FA where
    return = pure
    (FA fa) >>= f = FA $ \k -> fa (\a n -> gtimes n $ (unFA $ f a) k)

instance Alternative FA where
    empty = mempty
    (<|>) = (<>)

interpretFA :: Group g => FA g -> g
interpretFA (FA fa) = fa (flip gtimes)

reifyFA :: (Ord a) => FA a -> FreeAbelian a
reifyFA fa = interpretFA $ fmap singleton fa

reflectFA :: (Ord a) => FreeAbelian a -> FA a
reflectFA (FreeAbelian fa) = FA $ \k -> Map.foldMapWithKey k fa
