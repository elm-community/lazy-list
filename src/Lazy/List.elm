module Lazy.List
  ( LazyList (..)
  , empty
  , isEmpty
  , unique
  , length
  , member
  , sum
  , product
  , head
  , tail
  , cons
  , map
  , reduce
  , flatten
  , flatMap
  , append
  , reverse
  , take
  , takeWhile
  , drop
  , dropWhile
  , repeat
  , cycle
  , iterate
  , numbers
  , nth
  , keepIf
  , dropIf
  , foldl
  , foldr
  , andMap
  , andThen
  , map2
  , map3
  , map4
  , map5
  , zip
  , zip3
  , zip4
  , zip5
  , fromList
  , toList
  , fromArray
  , toArray
  , mapping
  , keeping
  , dropping
  ) where
{-| Lazy list implementation.

# Definition
@docs LazyList

# Basics
@docs empty, isEmpty, length, member

# Query List
@docs head, tail, nth

# Common List operations
@docs cons, map, reduce, flatten, flatMap, append, reverse, take, takeWhile, drop, dropWhile, unique

# Infinite List producers
@docs repeat, cycle, iterate, numbers

# Folds
@docs foldl, foldr, sum, product

# Chaining Lists
@docs andMap, andThen

# All the maps
@docs map2, map3, map4, map5

# All the zips
@docs zip, zip3, zip4, zip5

# Conversions
@docs toList, fromList, toArray, fromArray

# Useful Transducers
@docs mapping, keeping, dropping
-}

import Trampoline exposing (Trampoline(..), trampoline)
import Array      exposing (Array)


type LazyList a
  = Nil
  | Cons a (() -> LazyList a)

{-| Push or cons a value onto the front of a list.
-}
cons : a -> LazyList a -> LazyList a
cons a list =
  Cons a (\() -> list)

{-| The empty list.
-}
empty : LazyList a
empty =
  Nil

{-| Produce an infinite list of a certain value.
-}
repeat : a -> LazyList a
repeat a =
  Cons a (\() -> repeat a)


{-| Cycle a list n times
-}
cycle : Int -> LazyList a -> LazyList a
cycle n =
  repeat >> take n >> flatten


{-| Produce an infinite list of repeated applications of a function on a value.
-}
iterate : (a -> a) -> a -> LazyList a
iterate f a =
  Cons a (\() -> iterate f (f a))


{-| An infinite numbers of integers starting at 1.
-}
numbers : LazyList number
numbers =
  iterate ((+) 1) 1

{-} Ideal Recursive Implementation
take : Int -> LazyList a -> LazyList a
take n list = case list of
  Nil ->
    Nil
  Cons first rest ->
    if n <= 0
    then
      Nil
    else
      cons first (rest ())
-}

{-| Take n values from a list.
-}
take : Int -> LazyList a -> LazyList a
take n list = reverse <|
  trampoline (take' n list empty)


take' n list accum = case list of
  Nil ->
    Done accum
  Cons first rest ->
    if n <= 0
    then
      Done accum
    else
      Continue (\() -> take' (n - 1) (rest ()) (cons first accum))

{-| Take values from a list iteratively as long as the predicate is satisfied.
-}
takeWhile : (a -> Bool) -> LazyList a -> LazyList a
takeWhile predicate list = reverse <|
  trampoline (takeWhile' predicate list empty)

takeWhile' predicate list accum = case list of
  Nil ->
    Done accum
  Cons first rest ->
    if predicate first
    then
      Continue (\() -> takeWhile' predicate (rest ()) (cons first accum))
    else
      Done accum

{-| Drop values from a list iteratively as long as the predicate is satisfied
-}
dropWhile : (a -> Bool) -> LazyList a -> LazyList a
dropWhile predicate list =
  trampoline (dropWhile' predicate list)

dropWhile' predicate list = case list of
  Nil ->
    Done Nil
  Cons first rest ->
    if predicate first
    then
      Continue (\() -> dropWhile' predicate (rest ()))
    else
      Done list


{-| Drop n values from a list.
-}
drop : Int -> LazyList a -> LazyList a
drop n list =
  trampoline (drop' n list)

drop' n list = case list of
  Nil ->
    Done Nil
  Cons first rest ->
    if n <= 0
    then
      Done list
    else
      Continue (\() -> drop' (n - 1) (rest ()))

{-} Ideal recursive implementation
member : a -> LazyList a -> Bool
member a list = case list of
  Nil ->
    False
  Cons first rest ->
    first == a || member a (rest ())
-}

{-| Test if a value is a member of a list
-}
member : a -> LazyList a -> Bool
member a list =
  trampoline (member' a list)

member' a list = case list of
  Nil ->
    Done False
  Cons first rest ->
    if first == a
    then
      Done True
    else
      Continue (\() -> member' a (rest ()))

{-}
unique : LazyList a -> LazyList a
unique list = case list of
  Nil -> Nil
  Cons first rest ->
    let
        rest' = rest ()
    in
        if member first rest'
        then
          unique rest'
        else
          cons first rest'
-}

{-| Remove duplicates from a list.
-}
unique : LazyList a -> LazyList a
unique list = reverse <|
  trampoline (unique' list empty)

unique' list accum = case list of
  Nil ->
    Done accum
  Cons first rest ->
    let
        rest' = rest ()
    in
        if member first rest'
        then
          Continue (\() -> unique' rest' accum)
        else
          Continue (\() -> unique' rest' (cons first accum))




isEmpty : LazyList a -> Bool
isEmpty list = case list of
  Cons _ _ -> True
  _ -> False

length : LazyList a -> Int
length =
  reduce (\_ n -> n + 1) 0


sum : LazyList number -> number
sum =
  reduce (+) 0

product : LazyList number -> number
product =
  reduce (*) 1


{-} Ideal recursive implementation
nth : Int -> LazyList a -> Maybe a
nth n list = case list of
  Nil -> Nothing
  Cons first rest ->
    if | n <= 0 -> Nothing
       | n == 1 -> Just first
       | otherwise ->
          nth (n - 1) (rest ())
-}

{-| Get the nth value from a lazy list.
Note : nth considers lists to be 1-indexed.
-}
nth : Int -> LazyList a -> Maybe a
nth n list =
  trampoline (nth' n list)

nth' n list = case list of
  Nil -> Done Nothing
  Cons first rest ->
    if | n <= 0 -> Done Nothing
       | n == 1 -> Done (Just first)
       | otherwise ->
          Continue (\() -> nth' (n - 1) (rest ()))



{-| Map a function onto a list.
-}
map : (a -> b) -> LazyList a -> LazyList b
map f list = case list of
  Nil -> Nil
  Cons first rest ->
    Cons (f first) (\() -> map f (rest ()))

map2 : (a -> b -> c) -> LazyList a -> LazyList b -> LazyList c
map2 f l1 l2 = case l1 of
  Nil -> Nil
  Cons f1 r1 -> case l2 of
    Nil -> Nil
    Cons f2 r2 ->
      Cons (f f1 f2) (\() -> map2 f (r1 ()) (r2 ()))

map3 : (a -> b -> c -> d) -> LazyList a -> LazyList b -> LazyList c -> LazyList d
map3 f l1 l2 l3 =
  f
    `map`    l1
    `andMap` l2
    `andMap` l3

map4 : (a -> b -> c -> d -> e) -> LazyList a -> LazyList b -> LazyList c -> LazyList d -> LazyList e
map4 f l1 l2 l3 l4 =
  f
    `map`    l1
    `andMap` l2
    `andMap` l3
    `andMap` l4

map5 : (a -> b -> c -> d -> e -> f) -> LazyList a -> LazyList b -> LazyList c -> LazyList d -> LazyList e -> LazyList f
map5 f l1 l2 l3 l4 l5 =
  f
    `map`    l1
    `andMap` l2
    `andMap` l3
    `andMap` l4
    `andMap` l5

zip : LazyList a -> LazyList b -> LazyList (a, b)
zip =
  map2 (,)

zip3 : LazyList a -> LazyList b -> LazyList c -> LazyList (a, b, c)
zip3 =
  map3 (,,)

zip4 : LazyList a -> LazyList b -> LazyList c -> LazyList d -> LazyList (a, b, c, d)
zip4 =
  map4 (,,,)

zip5 : LazyList a -> LazyList b -> LazyList c -> LazyList d -> LazyList e -> LazyList (a, b, c, d, e)
zip5 =
  map5 (,,,,)

{-| Flatten a list of lists.
-}
flatten : LazyList (LazyList a) -> LazyList a
flatten =
  foldr append Nil

{-| Map a list constructor onto a list and flatten the result.
-}
flatMap : (a -> LazyList b) -> LazyList a -> LazyList b
flatMap f =
  flatten << map f


{-| This is useful when you want a version of `mapN` that is greater than 5.
-}
andMap : LazyList (a -> b) -> LazyList a -> LazyList b
andMap =
  map2 (<|)

{-| Chain a list and a list constructor.
-}
andThen : LazyList a -> (a -> LazyList b) -> LazyList b
andThen =
  flip flatMap

{-| Append two lists together.
-}
append : LazyList a -> LazyList a -> LazyList a
append l1 l2 = case l1 of
  Nil -> l2
  Cons first rest ->
    Cons first (\() -> append (rest ()) l2)

{-| Return the first element of a list. This returns `Nothing` if the list
is empty.
-}
head : LazyList a -> Maybe a
head list = case list of
  Nil       -> Nothing
  Cons a _  -> Just a

{-| Get the tail of a list. This returns `Nothing` if the list is empty.
-}
tail : LazyList a -> Maybe (LazyList a)
tail list = case list of
  Nil         -> Nothing
  Cons _ rest -> Just (rest ())

{-| Reverse a list.
-}
reverse : LazyList a -> LazyList a
reverse =
  reduce cons Nil

{-} Ideal recursive implementation
reduce : (a -> b -> b) -> b -> LazyList a -> b
reduce reducer initial list = case list of
  Nil -> initial
  Cons first rest ->
    reduce reducer (reducer first initial) (rest ())
-}

{-| Analogous to `List.foldl`. Reduce a list with a binary operation and an
initial value.
-}
reduce : (a -> b -> b) -> b -> LazyList a -> b
reduce reducer initial list =
  trampoline (reduce' reducer initial list)

{- Trampolined implementation
-}
reduce' : (a -> b -> b) -> b -> LazyList a -> Trampoline b
reduce' reducer initial list = case list of
  Nil ->
    Done initial
  Cons first rest ->
    Continue (\() -> reduce' reducer (reducer first initial) (rest ()))


{- Ideal recursive implementation.
keepIf : (a -> Bool) -> LazyList a -> LazyList a
keepIf predicate list = case list of
  Nil ->
    Nil
  Cons first rest ->
    if predicate first
    then
      Cons first (\() -> keepIf predicate (rest ()))
    else
      keepIf predicate (rest ())
-}

{-| Keep elements in list only if they satisfy a given predicate.
-}
keepIf : (a -> Bool) -> LazyList a -> LazyList a
keepIf predicate =
  foldr (keeping predicate cons) empty


{-| Drop elements from list only if they satisfy a given predicate.
-}
dropIf : (a -> Bool) -> LazyList a -> LazyList a
dropIf predicate =
  keepIf (\a -> not (predicate a))


{-| Fold from the left. Alias for `reduce`.
-}
foldl : (a -> b -> b) -> b -> LazyList a -> b
foldl =
  reduce

{-| Fold from the right.
-}
foldr : (a -> b -> b) -> b -> LazyList a -> b
foldr reducer initial =
  toArray >> Array.foldr reducer initial

{-} Ideal recursive implementation
toList : LazyList a -> List a
toList list = case list of
  Nil ->
    []
  Cons first rest ->
    first :: toList (rest ())
-}

{-| Convert a lazy list to a regular list.
-}
toList : LazyList a -> List a
toList list =
  trampoline (toList' [] list)

{- Trampolined implementation
-}
toList' accum list = case list of
  Nil ->
    Done accum
  Cons first rest ->
    Continue (\() -> toList' (accum ++ [first]) (rest ()))


{-| Convert a list to a lazy list.
-}
fromList : List a -> LazyList a
fromList =
  List.foldr cons empty


{-| Convert a lazy list to an array.
-}
toArray : LazyList a -> Array a
toArray list =
  trampoline (toArray' Array.empty list)

toArray' accum list = case list of
  Nil ->
    Done accum
  Cons first rest ->
    Continue (\() -> toArray' (Array.push first accum) (rest ()))


{-| Convert an array to a lazy list.
-}
fromArray : Array a -> LazyList a
fromArray =
  Array.foldr cons empty

-----------------
-- TRANSDUCERS --
-----------------

{-| Mapping transducer.
Map a function on a reducer.
-}
mapping : (a -> b) -> (b -> c -> c) -> (a -> c -> c)
mapping f reducer a c =
  reducer (f a) c

{-| Keeping transducer.
Analogous to `keepIf`.
-}
keeping : (a -> Bool) -> (a -> b -> b) -> (a -> b -> b)
keeping predicate reducer a b =
  if predicate a
  then
    reducer a b
  else
    b

{-| Dropping transducer.
Analogous to `dropIf`.
-}
dropping : (a -> Bool) -> (a -> b -> b) -> (a -> b -> b)
dropping predicate =
  keeping (\a -> not (predicate a))
