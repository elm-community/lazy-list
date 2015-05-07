module Lazy.List where

import Trampoline exposing (Trampoline(..), trampoline)
import Array      exposing (Array)
import List

type LazyListView a
  = Nil
  | Cons a (LazyList a)

type alias LazyList a = Lazy (LazyListView a)

type alias Lazy a = () -> a

force a = a ()

empty : LazyList a
empty _ = Nil

isEmpty : LazyList a -> Bool
isEmpty list =
  case force list of
    Nil -> True
    _ -> False

cons : a -> LazyList a -> LazyList a
cons a list _ =
  Cons a list


head : LazyList a -> Maybe a
head list =
  case force list of
    Nil -> Nothing
    Cons first _ -> Just first


tail : LazyList a -> Maybe (LazyList a)
tail list =
  case force list of
    Nil -> Nothing
    Cons _ rest -> Just rest



repeat : a -> LazyList a
repeat a _ =
  cons a (repeat a) ()



append : LazyList a -> LazyList a -> LazyList a
append list1 list2 _ =
  case force list1 of
    Nil -> force list2
    Cons first rest ->
      force (first ::: rest +++ list2)



intersperse : LazyList a -> LazyList a -> LazyList a
intersperse list1 list2 _ =
  case force list1 of
    Nil ->
      force list2
    Cons first1 rest1 ->
      case force list2 of
        Nil ->
          force list1
        Cons first2 rest2 ->
          force (first1 ::: first2 ::: intersperse rest1 rest2)




cycle : LazyList a -> LazyList a
cycle list =
  list +++ (\() ->
    force (cycle list)
  )

-- TODO: Trampoline
iterate : (a -> a) -> a -> LazyList a
iterate f a _ =
  cons a (iterate f (f a)) ()

numbers : LazyList number
numbers =
  iterate ((+) 1) 1


-- TODO: Trampoline
take : Int -> LazyList a -> LazyList a
take n list _ =
  if n <= 0
  then
    Nil
  else
    case force list of
      Nil -> Nil
      Cons first rest ->
        cons first (take (n - 1) rest) ()



-- TODO: Trampoline
takeWhile : (a -> Bool) -> LazyList a -> LazyList a
takeWhile predicate list _ =
  case force list of
    Nil -> Nil
    Cons first rest ->
      if predicate first
      then
        Cons first (takeWhile predicate rest)
      else
        Nil


-- TODO: Trampoline
drop : Int -> LazyList a -> LazyList a
drop n list _ =
  if n <= 0
  then
    list ()
  else
    case force list of
      Nil -> Nil
      Cons first rest ->
        drop (n - 1) rest ()


-- TODO: Trampoline
dropWhile : (a -> Bool) -> LazyList a -> LazyList a
dropWhile predicate list _ =
  case force list of
    Nil -> Nil
    Cons first rest ->
      if predicate first
      then
        dropWhile predicate rest ()
      else
        list ()


-- TODO: Trampoline
member : a -> LazyList a -> Bool
member a list =
  case force list of
    Nil -> False
    Cons first rest ->
      first == a || member a rest


length : LazyList a -> Int
length =
  reduce (\_ n -> n + 1) 0


-- TODO: Trampoline
unique : LazyList a -> LazyList a
unique list _ =
  case force list of
    Nil -> Nil
    Cons first rest ->
      if first `member` rest
      then
        unique rest ()
      else
        Cons first (unique rest)


-- TODO: Trampoline
keepIf : (a -> Bool) -> LazyList a -> LazyList a
keepIf predicate list _ =
  case force list of
    Nil -> Nil
    Cons first rest ->
      if predicate first
      then
        Cons first (keepIf predicate rest)
      else
        keepIf predicate rest ()


-- TODO: Trampoline
dropIf : (a -> Bool) -> LazyList a -> LazyList a
dropIf predicate =
  keepIf (\n -> not (predicate n))


-- TODO: Trampoline
reduce : (a -> b -> b) -> b -> LazyList a -> b
reduce reducer b list =
  case force list of
    Nil -> b
    Cons first rest ->
      reduce reducer (reducer first b) rest


foldl : (a -> b -> b) -> b -> LazyList a -> b
foldl = reduce


foldr : (a -> b -> b) -> b -> LazyList a -> b
foldr reducer b list =
  Array.foldr reducer b (toArray list)

sum : LazyList number -> number
sum =
  reduce (+) 0

product : LazyList number -> number
product =
  reduce (*) 1


-- TODO: Trampoline
flatten : LazyList (LazyList a) -> LazyList a
flatten list _ =
  case force list of
    Nil -> Nil
    Cons first rest ->
      force (first +++ flatten rest)


flatMap : (a -> LazyList b) -> LazyList a -> LazyList b
flatMap f =
  map f >> flatten

andThen : LazyList a -> (a -> LazyList b) -> LazyList b
andThen =
  flip flatMap


reverse : LazyList a -> LazyList a
reverse =
  reduce cons empty

-- TODO: Trampoline
map : (a -> b) -> LazyList a -> LazyList b
map f list _ =
  case force list of
    Nil -> Nil
    Cons first rest ->
      Cons (f first) (map f rest)

-- TODO: Trampoline
map2 : (a -> b -> c) -> LazyList a -> LazyList b -> LazyList c
map2 f list1 list2 _ =
  case force list1 of
    Nil -> Nil
    Cons first1 rest1 ->
      case force list2 of
        Nil -> Nil
        Cons first2 rest2 ->
          Cons (f first1 first2) (map2 f rest1 rest2)


andMap : LazyList (a -> b) -> LazyList a -> LazyList b
andMap =
  map2 (<|)


map3 : (a -> b -> c -> d) -> LazyList a -> LazyList b -> LazyList c -> LazyList d
map3 f l1 l2 l3 =
  f
    `map` l1
    `andMap` l2
    `andMap` l3

map4 : (a -> b -> c -> d -> e) -> LazyList a -> LazyList b -> LazyList c -> LazyList d -> LazyList e
map4 f l1 l2 l3 l4 =
  f
    `map` l1
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


-- TODO: Trampoline
toList : LazyList a -> List a
toList list =
  case force list of
    Nil -> []
    Cons first rest ->
      first :: toList rest


fromList : List a -> LazyList a
fromList =
  List.foldr cons empty

-- TODO: Trampoline
toArray : LazyList a -> Array a
toArray list =
  case force list of
    Nil -> Array.empty
    Cons first rest ->
      Array.append (Array.push first Array.empty) (toArray rest)



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




---------------------
-- INFIX OPERATORS --
---------------------

infixr 5 :::

(:::) : a -> LazyList a -> LazyList a
(:::) = cons


infixr 5 +++

(+++) : LazyList a -> LazyList a -> LazyList a
(+++) = append
