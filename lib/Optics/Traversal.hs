module Optics.Traversal where

import Data.Functor.Const
import Data.Functor.Identity

import Optics.Base
import Optics.Fold
import Optics.Setter

-- | Traversals collect or set zero or more values.
newtype Traversal s t a b = Traversal
  { runTraversal :: forall f. Applicative f => (a -> f b) -> s -> f t
  }

instance Weaken Traversal Fold where
  weaken (Traversal o) = Fold \f -> getConst . o (Const . f)

instance Weaken Traversal Setter where
  weaken (Traversal o) = Setter \f -> runIdentity . o (Identity . f)

instance Compose Traversal Traversal Traversal where
  Traversal f ◊ Traversal g = Traversal (f . g)

instance Compose Fold Traversal Fold
instance Compose Traversal Fold Fold
instance Compose AffineFold Traversal Fold
instance Compose Traversal AffineFold Fold
instance Compose Setter Traversal Setter
instance Compose Traversal Setter Setter

traversed :: Traversable f => Traversal (f a) (f b) a b
traversed = Traversal traverse

traverseOf :: (Weaken o Traversal, Applicative f) => o s t a b -> (a -> f b) -> s -> f t
traverseOf = runTraversal . weaken

-- | Traversals that target at most one value.
newtype AffineTraversal s t a b = AffineTraversal { runAffineTraversal :: s -> Either t (a, b -> t) }

instance Weaken AffineTraversal Fold where
  weaken = weakenVia @AffineFold

instance Weaken AffineTraversal AffineFold where
  weaken (AffineTraversal o) = AffineFold \s -> case o s of
    Left _ -> Nothing
    Right (a, _) -> Just a

instance Weaken AffineTraversal Setter where
  weaken = weakenVia @Traversal

instance Weaken AffineTraversal Traversal where
  weaken (AffineTraversal o) = Traversal \f s ->
    case o s of
      Left t -> pure t
      Right (a, bt) -> bt <$> f a

instance Compose AffineTraversal AffineTraversal AffineTraversal where
  AffineTraversal o ◊ AffineTraversal p = AffineTraversal \s ->
    case o s of
      Left t -> Left t
      Right (x, yt) -> case p x of
        Left y -> Left (yt y)
        Right (a, by) -> Right (a, yt . by)

instance Compose Fold AffineTraversal Fold
instance Compose AffineTraversal Fold Fold
instance Compose AffineFold AffineTraversal AffineFold
instance Compose AffineTraversal AffineFold AffineFold 
instance Compose Setter AffineTraversal Setter
instance Compose AffineTraversal Setter Setter
instance Compose Traversal AffineTraversal Traversal
instance Compose AffineTraversal Traversal Traversal