module Data.Array.Builder.Cons
  ( build
  , Builder
  , cons
  ) where

import Prelude
import Data.Array (reverse)
import Data.Array.Builders.Internal (unsafeCons)

newtype Builder a
  = Builder { prepend :: Array a -> Array a, depth :: Int }

instance semigroupBuilder :: Semigroup (Builder a) where
  append (Builder b1) (Builder b2) =
    if b1.depth + b2.depth > 512 then
      let
        prefix = (b2.prepend <<< b1.prepend) []

        prepend arr = arr <> prefix
      in
        Builder { prepend, depth: 1 }
    else
      Builder { prepend: b2.prepend <<< b1.prepend, depth: b1.depth + b2.depth }

instance monoidBuilder :: Monoid (Builder a) where
  mempty = Builder { prepend: identity, depth: 1 }

cons :: forall a. a -> Builder a
cons a = Builder { prepend: unsafeCons a, depth: 1 }

build :: forall a. Builder a -> Array a
build (Builder { prepend }) = (reverse $ prepend [])
