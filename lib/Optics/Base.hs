module Optics.Base where

import Data.Kind

type Optic = Type -> Type -> Type -> Type -> Type

-- | Weakening optics
class Weaken o p where
  weaken :: o s t a b -> p s t a b

instance {-# OVERLAPPABLE #-} Weaken o o where
  weaken = id
  {-# INLINE weaken #-}

weakenVia :: forall v o p s t a b. (Weaken o v, Weaken v p) => o s t a b -> p s t a b
weakenVia = weaken @v @p . weaken @o @v
{-# INLINE weakenVia #-}

-- | Composing optics
class Compose o p q | o p -> q where
  (◊) :: o s t x y -> p x y a b -> q s t a b
  default (◊) :: (Weaken o q, Weaken p q, Compose q q q) => o s t x y -> p x y a b -> q s t a b
  o ◊ p = weaken @o @q o ◊ weaken @p @q p
  {-# INLINE (◊) #-}
