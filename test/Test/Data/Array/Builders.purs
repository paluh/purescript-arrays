module Test.Data.Array.Builder (testArrayBuilder) where

import Prelude

import Data.Array (cons, fromFoldable) as Array
import Data.Array (foldMap, foldr)
import Data.Array.Builder.Cons (cons, build) as Cons
import Data.Array.Builder.Overflowing (build, cons, snoc) as Overflowing
import Data.DateTime.Instant (unInstant)
import Data.Foldable (foldl)
import Data.Newtype (un)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Now (now)
import Test.Assert (assert)

largeArr :: Array Int
largeArr = foldl (\r _ -> r <> r) [ 0, 1 ] [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 ]

timeit :: (Unit -> Effect Unit) -> Effect Unit
timeit =
  let
    toNumber = un Milliseconds <<< unInstant
  in \action -> do
    start <- now
    void $ action unit
    end <- now
    log $ show (toNumber end - toNumber start)

testArrayBuilder :: Effect Unit
testArrayBuilder = do
  log "Overflowing.Builder cons chain builds array according to the call order"
  assert $ Overflowing.build (Overflowing.cons 1 <> Overflowing.cons 2 <> Overflowing.cons 3) == [ 1, 2, 3 ]
  log "Overflowing.Builder snoc chain builds array with reverse order"
  assert $ Overflowing.build (Overflowing.snoc 1 <> Overflowing.snoc 2 <> Overflowing.snoc 3) == [ 3, 2, 1 ]
  log "Cons.Builder cons chain builds array according to the call order"
  assert
    $ Cons.build
        (Cons.cons 1 <> Cons.cons 2 <> Cons.cons 3 <> Cons.cons 4 <> Cons.cons 5 <> Cons.cons 6 <> Cons.cons 7 <> Cons.cons 8)
    == [ 1, 2, 3, 4, 5, 6, 7, 8 ]

  log "Cons.Builder performance"
  timeit \_ -> assert $ Cons.build (foldMap Cons.cons largeArr) == largeArr

  -- log "Overflowing.Builder performance"
  -- timeit \_ -> assert $ Overflowing.build (foldMap Overflowing.cons largeArr) == largeArr

  log "Array.cons performance"
  timeit \_ -> assert $ (foldr Array.cons [] largeArr) == largeArr

  log "Array.fromFoldable performance"
  timeit \_ -> assert $ (Array.fromFoldable largeArr) == largeArr
