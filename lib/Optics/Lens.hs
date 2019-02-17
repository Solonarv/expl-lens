module Optics.Lens where

import Optics.Base
import Optics.Fold
import Optics.Getter
import Optics.Setter
import Optics.Traversal

-- | Reified fields.
newtype Lens s t a b = Lens { runLens :: s -> (a, b -> t) }

instance Weaken Lens Fold where
  weaken = weakenVia @Getter

instance Weaken Lens AffineFold where
  weaken = weakenVia @Getter

instance Weaken Lens Getter where
  weaken (Lens l) = Getter (fst . l)

instance Weaken Lens Setter where
  weaken = weakenVia @AffineTraversal

instance Weaken Lens Traversal where
  weaken = weakenVia @AffineTraversal

instance Weaken Lens AffineTraversal where
  weaken (Lens l) = AffineTraversal (Right . l)

instance Compose Lens Lens Lens where
  Lens o ◊ Lens p = Lens \s -> case o s of
    (x, yt) -> case p x of
      (a, by) -> (a, yt . by)

instance Compose Lens Fold Fold
instance Compose Fold Lens Fold
instance Compose Lens AffineFold AffineFold
instance Compose AffineFold Lens AffineFold
instance Compose Lens Getter Getter
instance Compose Getter Lens Getter
instance Compose Lens Setter Setter
instance Compose Setter Lens Setter
instance Compose Traversal Lens Traversal
instance Compose Lens Traversal Traversal
instance Compose AffineTraversal Lens AffineTraversal
instance Compose Lens AffineTraversal AffineTraversal

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt = Lens \s -> (sa s, sbt s)