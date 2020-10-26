module Data.Array.Builders.Internal where

foreign import unsafeCons :: forall a. a -> Array a -> Array a

foreign import unsafeSnoc :: forall a. a -> Array a -> Array a
