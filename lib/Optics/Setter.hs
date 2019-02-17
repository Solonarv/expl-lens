module Optics.Setter where

import Optics.Base

-- | Reified field updates.
newtype Setter s t a b = Setter { runSetter :: (a -> b) -> s -> t }

instance Compose Setter Setter Setter where
  Setter f ◊ Setter g = Setter (f . g)

over, (%~) :: Weaken o Setter => o s t a b -> (a -> b) -> s -> t
over = runSetter . weaken
(%~) = over

set, (.~) :: Weaken o Setter => o s t a b -> b -> s -> t
set o = over o . const
(.~) = set
