module Optics.Review where

import Optics.Base
import Optics.Getter

-- | Reified constructors.
newtype Review s t a b = Review { runReview :: b -> t }

instance Compose Review Review Review where
  Review f ◊ Review g = Review (f . g)

re :: Weaken o Review => o s t a b -> Getter b a t s
re = Getter . runReview . weaken

review, (#) :: Weaken o Review => o s t a b -> b -> t
review = runReview . weaken
(#) = review