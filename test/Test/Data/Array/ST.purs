module Test.Data.Array.ST (testArrayST) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Data.Array.ST (STArray, emptySTArray, freeze, peekSTArray, pokeSTArray, pushAllSTArray, pushSTArray, spliceSTArray, thaw, toAssocArray, unsafeThaw, unsafeFreeze)
import Data.Foldable (all)
import Data.Maybe (Maybe(..), isNothing)
import Test.Assert (assert)

run :: forall a. (forall r. ST r (STArray r a)) -> Array a
run act = ST.run (act >>= unsafeFreeze)

testArrayST :: Effect Unit
testArrayST = do

  log "emptySTArray should produce an empty array"

  assert $ run emptySTArray == nil

  log "thaw should produce an STArray from a standard array"

  assert $ run (thaw [1, 2, 3]) == [1, 2, 3]

  log "freeze should produce a standard array from an STArray"

  assert $ ST.run (do
    arr <- thaw [1, 2, 3]
    freeze arr) == [1, 2, 3]

  log "unsafeThaw should produce an STArray from a standard array"

  assert $ run (unsafeThaw [1, 2, 3]) == [1, 2, 3]

  log "pushSTArray should append a value to the end of the array"

  assert $ run (do
    arr <- emptySTArray
    void $ pushSTArray arr 1
    void $ pushSTArray arr 2
    pure arr) == [1, 2]

  assert $ run (do
    arr <- thaw [1, 2, 3]
    void $ pushSTArray arr 4
    pure arr) == [1, 2, 3, 4]

  log "pushSTArray should return the new length of the array"

  assert $ ST.run (do
    arr <- thaw [unit, unit, unit]
    pushSTArray arr unit) == 4

  log "pushAllSTArray should append multiple values to the end of the array"

  assert $ run (do
    arr <- emptySTArray
    void $ pushAllSTArray arr [1, 2]
    pure arr) == [1, 2]

  assert $ run (do
    arr <- thaw [1, 2, 3]
    void $ pushAllSTArray arr [4, 5, 6]
    pure arr) == [1, 2, 3, 4, 5, 6]

  log "pushAllSTArray should return the new length of the array"

  assert $ ST.run (do
    arr <- thaw [unit, unit, unit]
    pushAllSTArray arr [unit, unit]) == 5

  log "peekSTArray should return Nothing when peeking a value outside the array bounds"

  assert $ isNothing $ ST.run (do
    arr <- emptySTArray
    peekSTArray arr 0)

  assert $ isNothing $ ST.run (do
    arr <- thaw [1]
    peekSTArray arr 1)

  assert $ isNothing $ ST.run (do
    arr <- emptySTArray
    peekSTArray arr (-1))

  log "peekSTArray should return the value at the specified index"

  assert $ ST.run (do
    arr <- thaw [1]
    peekSTArray arr 0) == Just 1

  assert $ ST.run (do
    arr <- thaw [1, 2, 3]
    peekSTArray arr 2) == Just 3

  log "pokeSTArray should return true when a value has been updated succesfully"

  assert $ ST.run (do
    arr <- thaw [1]
    pokeSTArray arr 0 10)

  assert $ ST.run (do
    arr <- thaw [1, 2, 3]
    pokeSTArray arr 2 30)

  log "pokeSTArray should return false when attempting to modify a value outside the array bounds"

  assert $ not $ ST.run (do
    arr <- emptySTArray
    pokeSTArray arr 0 10)

  assert $ not $ ST.run (do
    arr <- thaw [1, 2, 3]
    pokeSTArray arr 3 100)

  assert $ not $ ST.run (do
    arr <- thaw [1, 2, 3]
    pokeSTArray arr (-1) 100)

  log "pokeSTArray should replace the value at the specified index"

  assert $ run (do
    arr <- thaw [1]
    void $ pokeSTArray arr 0 10
    pure arr) == [10]

  log "pokeSTArray should do nothing when attempting to modify a value outside the array bounds"

  assert $ run (do
    arr <- thaw [1]
    void $ pokeSTArray arr 1 2
    pure arr) == [1]

  log "spliceSTArray should be able to delete multiple items at a specified index"

  assert $ run (do
    arr <- thaw [1, 2, 3, 4, 5]
    void $ spliceSTArray arr 1 3 []
    pure arr) == [1, 5]

  log "spliceSTArray should return the items removed"

  assert $ ST.run (do
    arr <- thaw [1, 2, 3, 4, 5]
    spliceSTArray arr 1 3 []) == [2, 3, 4]

  log "spliceSTArray should be able to insert multiple items at a specified index"

  assert $ run (do
    arr <- thaw [1, 2, 3, 4, 5]
    void $ spliceSTArray arr 1 0 [0, 100]
    pure arr) == [1, 0, 100, 2, 3, 4, 5]

  log "spliceSTArray should be able to delete and insert at the same time"

  assert $ run (do
    arr <- thaw [1, 2, 3, 4, 5]
    void $ spliceSTArray arr 1 2 [0, 100]
    pure arr) == [1, 0, 100, 4, 5]

  log "toAssocArray should return all items in the array with the correct indices and values"

  assert $ all (\{ value: v, index: i } -> v == i + 1) $ ST.run (do
    arr <- thaw [1, 2, 3, 4, 5]
    toAssocArray arr)

  assert $ all (\{ value: v, index: i } -> v == (i + 1) * 10) $ ST.run (do
    arr <- thaw [10, 20, 30, 40, 50]
    toAssocArray arr)

nil :: Array Int
nil = []
