module Data.Array.Builder.Overflowing
  ( build
  , Builder
  , cons
  , snoc
  ) where

import Prelude
import Data.Array.Builders.Internal (unsafeCons, unsafeSnoc)

newtype Builder a
  = Builder (Array a -> Array a)

instance semigroupBuilder :: Semigroup (Builder a) where
  append (Builder b1) (Builder b2) = Builder (b1 <<< b2)

instance monoidBuilder :: Monoid (Builder a) where
  mempty = Builder identity

cons :: forall a. a -> Builder a
cons a = Builder (unsafeCons a)

snoc :: forall a. a -> Builder a
snoc a = Builder (unsafeSnoc a)

build :: forall a. Builder a -> Array a
build (Builder f) = f []
