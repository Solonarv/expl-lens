{-# LANGUAGE UndecidableInstances #-}
module Optics.Iso where

import Optics.Base
import Optics.Fold
import Optics.Getter
import Optics.Lens
import Optics.Prism
import Optics.Review
import Optics.Setter
import Optics.Traversal

data Iso s t a b = Iso { isoTo :: s -> a, isoFrom :: b -> t }

instance Weaken Iso Fold where
  weaken = weakenVia @Getter

instance Weaken Iso AffineFold where
  weaken = weakenVia @Getter

instance Weaken Iso Getter where
  weaken = Getter . isoTo

instance Weaken Iso Lens where
  weaken i = Lens \s -> (isoTo i s, isoFrom i)

instance Weaken Iso Prism where
  weaken i = Prism (Right . isoTo i) (isoFrom i)

instance Weaken Iso Review where
  weaken = Review . isoFrom

instance Weaken Iso Setter where
  weaken = weakenVia @Lens

instance Weaken Iso Traversal where
  weaken = weakenVia @AffineTraversal

instance Weaken Iso AffineTraversal where
  weaken i = AffineTraversal \s -> Right (isoTo i s, isoFrom i)

instance {-# overlapping #-} Compose Iso Iso Iso where
  i ◊ j = Iso (isoTo j . isoTo i) (isoFrom i . isoFrom j)

-- A blessing! Catch-all instances!
instance (Weaken Iso o, Compose o o o) => Compose Iso o o
instance (Weaken Iso o, Compose o o o) => Compose o Iso o

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso = Iso

from :: Iso s t a b -> Iso b a t s
from (Iso t f) = Iso f t