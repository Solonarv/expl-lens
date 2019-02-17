module Optics.Fold where

import Control.Monad
import Data.Monoid

import Optics.Base

-- | Get zero or more values.
newtype Fold s t a b = Fold { runFold :: forall m. Monoid m => (a -> m) -> s -> m }

instance Compose Fold Fold Fold where
  Fold f ◊ Fold g = Fold (f . g)

folded :: Foldable f => Fold (f a) (f b) a b
folded = Fold foldMap

foldMapOf :: (Weaken o Fold, Monoid m) => o s t a b -> (a -> m) -> s -> m
foldMapOf = runFold . weaken

toListOf :: Weaken o Fold => o s t a b -> s -> [a]
toListOf o s = foldMapOf o (\a -> Endo (a:)) s `appEndo` []
(^..) :: Weaken o Fold => s -> o s t a b -> [a]
(^..) = flip toListOf

preview :: Weaken o Fold => o s t a b -> s -> Maybe a
preview (weaken -> o) = getFirst . runFold o (First . Just)
(^?) :: Weaken o Fold => s -> o s t a b -> Maybe a
(^?) = flip preview

-- | Get at most one value.
newtype AffineFold s t a b = AffineFold { runAffineFold :: s -> Maybe a }

instance Weaken AffineFold Fold where
  weaken (AffineFold o) = Fold \f s -> foldMap f (o s)

instance Compose AffineFold AffineFold AffineFold where
  AffineFold sx ◊ AffineFold xa = AffineFold (sx >=> xa)

instance Compose AffineFold Fold Fold
instance Compose Fold AffineFold Fold