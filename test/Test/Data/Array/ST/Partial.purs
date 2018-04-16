module Test.Data.Array.ST.Partial (testArraySTPartial) where

import Prelude

import Control.Monad.ST as ST
import Data.Array.ST (thaw, unsafeFreeze)
import Data.Array.ST.Partial (peekSTArray, pokeSTArray)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert)

testArraySTPartial :: Effect Unit
testArraySTPartial = do

  log "peekSTArray should return the value at the specified index"
  assert $ 2 == ST.run do
    a <- thaw [1, 2, 3]
    unsafePartial $ peekSTArray a 1

  log "pokeSTArray should modify the value at the specified index"
  assert $ [1, 4, 3] == ST.run do
    a <- thaw [1, 2, 3]
    unsafePartial $ pokeSTArray a 1 4
    unsafeFreeze a
