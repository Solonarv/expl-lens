{-# LANGUAGE TupleSections #-}
module Optics.Prism where

import Data.Bifunctor

import Optics.Base
import Optics.Fold
import Optics.Review
import Optics.Setter
import Optics.Traversal

-- only needed for Compose instances
import Optics.Lens

-- | Reified pattern matches.
data Prism s t a b = Prism { matchPrism :: s -> Either t a, rePrism :: b -> t }

instance Weaken Prism Fold where
  weaken = weakenVia @AffineTraversal

instance Weaken Prism AffineFold where
  weaken = weakenVia @AffineTraversal

instance Weaken Prism Review where
  weaken (Prism sta bt) = Review bt

instance Weaken Prism Setter where
  weaken = weakenVia @AffineTraversal

instance Weaken Prism Traversal where
  weaken = weakenVia @AffineTraversal

instance Weaken Prism AffineTraversal where
  weaken (Prism sta bt) = AffineTraversal (second (, bt) . sta)

instance Compose Prism Prism Prism where
  o ◊ p = Prism
    { matchPrism = \s -> case matchPrism o s of
        Left t -> Left t
        Right x -> case matchPrism p x of
          Left y -> Left (rePrism o y)
          Right a -> Right a
    , rePrism = rePrism o . rePrism p
    }

instance Compose Prism Fold Fold
instance Compose Fold Prism Fold
instance Compose Prism AffineFold AffineFold
instance Compose AffineFold Prism AffineFold
instance Compose Prism Review Review
instance Compose Review Prism Review
instance Compose Prism Setter Setter
instance Compose Setter Prism Setter
instance Compose Prism Traversal Traversal
instance Compose Traversal Prism Traversal
instance Compose Prism AffineTraversal AffineTraversal
instance Compose AffineTraversal Prism AffineTraversal
instance Compose Prism Lens AffineTraversal
instance Compose Lens Prism AffineTraversal

match :: Weaken o Prism => o s t a b -> s -> Either t a
match = matchPrism . weaken