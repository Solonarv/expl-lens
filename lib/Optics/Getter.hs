module Optics.Getter where

import Optics.Base
import Optics.Fold

-- | Get exactly one value.
newtype Getter s t a b = Getter { runGetter :: s -> a }

instance Weaken Getter Fold where
  weaken = weakenVia @AffineFold

instance Weaken Getter AffineFold where
  weaken (Getter sa) = AffineFold (Just . sa)

instance Compose Getter Getter Getter where
  Getter sx ◊ Getter xa = Getter (xa . sx)

instance Compose AffineFold Getter AffineFold
instance Compose Getter AffineFold AffineFold
instance Compose Fold Getter Fold
instance Compose Getter Fold Fold

to :: (s -> a) -> Getter s t a b
to = Getter

view :: Weaken o Getter => o s t a b -> s -> a
view = runGetter . weaken

(^.) :: Weaken o Getter => s -> o s t a b -> a
(^.) = flip view

