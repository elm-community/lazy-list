module Lazy.List where
{-| Lazy list implementation in Elm.

# Types
@docs LazyList, LazyListView, Lazy

# Force lazy evaluation
@docs force

# Constructors
@docs cons, empty

# Query operations
@docs isEmpty, head, tail, member, length

# Conversions
@docs toList, fromList, toArray, fromArray

# Map-reduce et al.
@docs map, zip, reduce, flatten, flatMap, append, foldl, foldr

# Common operations
@docs intersperse, reverse, cycle, iterate, repeat, take, takeWhile, drop, dropWhile

# Filtering operations
@docs keepIf, dropIf, unique

# Chaining operations
@docs andMap, andThen

# Useful stuff
@docs numbers, sum, product

# All the maps!
@docs map2, map3, map4, map5

# All the zips!
@docs zip3, zip4, zip5

# Common Transducers
@docs mapping, keeping, dropping

# Infix Operators
@docs (:::), (+++)

# Random Generation Helpers
@docs genList, lazylist

-}

import Trampoline exposing (Trampoline(..), trampoline)
import Array      exposing (Array)
import List
import Random exposing (Generator, Seed)
import Random.Extra as Random

{-| Analogous to `List` type. This is the actual implementation type for the
`LazyList` type. This type is exposed to the user if the user so wishes to
do pattern matching or understand how the list type works. It is not
recommended to work with this type directly. Try working solely with the
provided functions in the package.
-}
type LazyListView a
  = Nil
  | Cons a (LazyList a)

{-| Lazy List type.
-}
type alias LazyList a = Lazy (LazyListView a)

{-| A Lazy value is a delayed value. This delay is induced by the lambda.
This means that what's inside the lambda could require arbitrary amounts of
compute power but will not do anything unless you manually `force` the
evaluation of this value.
-}
type alias Lazy a = () -> a


{-| Lazily randomly generate `n` many values.
-}
genList : Int -> Generator a -> Seed -> (LazyList a, Seed)
genList n generator seed =
  if n <= 0
  then
      (empty, seed)
  else
      let (value, nextSeed) = Random.generate generator seed
      in
          (\() ->
              Cons value (fst (genList (n - 1) generator nextSeed))
          , nextSeed
          )

{-| Analogue of `Random.list` generator constructor.
-}
lazylist : Int -> Generator a -> Generator (LazyList a)
lazylist n generator =
  Random.customGenerator (genList n generator)


{-| Force evaluation of a lazy value.
Be very careful when using this function. Lazy values can potentially be
infinitely recursive or be very computationally intensive. Try to avoid calling
this function if you can, and plan its use if you must.
-}
force : Lazy a -> a
force a = a ()

{-| Create an empty list.
-}
empty : LazyList a
empty _ = Nil


{-| Detect if a list is empty or not.
-}
isEmpty : LazyList a -> Bool
isEmpty list =
  case force list of
    Nil -> True
    _ -> False

{-| Add a value to the front of a list.
-}
cons : a -> LazyList a -> LazyList a
cons a list _ =
  Cons a list


{-| Get the head of a list.
-}
head : LazyList a -> Maybe a
head list =
  case force list of
    Nil -> Nothing
    Cons first _ -> Just first


{-| Get the tail of a list.
-}
tail : LazyList a -> Maybe (LazyList a)
tail list =
  case force list of
    Nil -> Nothing
    Cons _ rest -> Just rest



{-| Repeat a value ad infinitum.
Be careful when you use this. The result of this is a truly infinite list.
Do not try calling `reduce` or `toList` on an infinite list as it'll never
finish computing. Make sure you then filter it down to a finite list with `head`
or `take` or something.
-}
repeat : a -> LazyList a
repeat a _ =
  cons a (repeat a) ()


{-| Append a list to another list.
-}
append : LazyList a -> LazyList a -> LazyList a
append list1 list2 _ =
  case force list1 of
    Nil -> force list2
    Cons first rest ->
      force (first ::: rest +++ list2)


{-| Intersperse the elements of a list in another list. The two lists get
interleaved at the end.
-}
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



{-| Take a list and repeat it ad infinitum. This cycles a finite list
by putting the front after the end of the list. This results in a no-op in
the case of an infinite list.
-}
cycle : LazyList a -> LazyList a
cycle list =
  list +++ (\() ->
    force (cycle list)
  )

-- TODO: Trampoline
{-| Create an infinite list of applications of a function on some value.

Equivalent to:

    x ::: f x ::: f (f x) ::: f (f (f x)) ::: ... -- etc...
-}
iterate : (a -> a) -> a -> LazyList a
iterate f a _ =
  cons a (iterate f (f a)) ()

{-| The list of counting numbers.

i.e.:

    1 ::: 2 ::: 3 ::: 4 ::: 5 ::: ... -- etc...
-}
numbers : LazyList number
numbers =
  iterate ((+) 1) 1


-- TODO: Trampoline
{-| Take at most `n` many values from a list.
-}
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
{-| Take elements from a list as long as the predicate is satisfied.
-}
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
{-| Drop at most `n` many values from a list.
-}
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
{-| Drop elements from a list as long as the predicate is satisfied.
-}
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
{-| Test if a value is a member of a list.
-}
member : a -> LazyList a -> Bool
member a list =
  case force list of
    Nil -> False
    Cons first rest ->
      first == a || member a rest


{-| Get the length of a lazy list.

Warning: This will not terminate if the list is infinite.
-}
length : LazyList a -> Int
length =
  reduce (\_ n -> n + 1) 0


-- TODO: Trampoline
{-| Remove all duplicates from a list and return a list of distinct elements.
-}
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
{-| Keep all elements in a list that satisfy the given predicate.
-}
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
{-| Drop all elements in a list that satisfy the given predicate.
-}
dropIf : (a -> Bool) -> LazyList a -> LazyList a
dropIf predicate =
  keepIf (\n -> not (predicate n))


-- TODO: Trampoline
{-| Reduce a list with a given reducer and an initial value.

Example :
    reduce (+) 0 (1 ::: 2 ::: 3 ::: 4 ::: empty) == 10
-}
reduce : (a -> b -> b) -> b -> LazyList a -> b
reduce reducer b list =
  case force list of
    Nil -> b
    Cons first rest ->
      reduce reducer (reducer first b) rest


{-| Analogous to `List.foldl`. Is an alias for `reduce`.
-}
foldl : (a -> b -> b) -> b -> LazyList a -> b
foldl = reduce


{-| Analogous to `List.foldr`.
-}
foldr : (a -> b -> b) -> b -> LazyList a -> b
foldr reducer b list =
  Array.foldr reducer b (toArray list)

{-| Get the sum of a list of numbers.
-}
sum : LazyList number -> number
sum =
  reduce (+) 0

{-| Get the product of a list of numbers.
-}
product : LazyList number -> number
product =
  reduce (*) 1


-- TODO: Trampoline
{-| Flatten a list of lists into a single list by appending all the inner
lists into one big list.
-}
flatten : LazyList (LazyList a) -> LazyList a
flatten list _ =
  case force list of
    Nil -> Nil
    Cons first rest ->
      force (first +++ flatten rest)


{-| Map then flatten.
-}
flatMap : (a -> LazyList b) -> LazyList a -> LazyList b
flatMap f =
  map f >> flatten

{-| Chain list producing operations.
-}
andThen : LazyList a -> (a -> LazyList b) -> LazyList b
andThen =
  flip flatMap


{-| Reverse a list.
-}
reverse : LazyList a -> LazyList a
reverse =
  reduce cons empty

-- TODO: Trampoline
{-| Map a function to a list.
-}
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


{-| Known as `mapN` in some circles. Allows you to apply `map` in cases
where then number of arguments are greater than 5.
-}
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
{-| Convert a lazy list to a normal list.
-}
toList : LazyList a -> List a
toList list =
  case force list of
    Nil -> []
    Cons first rest ->
      first :: toList rest


{-| Convert a normal list to a lazy list.
-}
fromList : List a -> LazyList a
fromList =
  List.foldr cons empty

-- TODO: Trampoline
{-| Convert a lazy list to an array.
-}
toArray : LazyList a -> Array a
toArray list =
  case force list of
    Nil -> Array.empty
    Cons first rest ->
      Array.append (Array.push first Array.empty) (toArray rest)


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




---------------------
-- INFIX OPERATORS --
---------------------

infixr 5 :::

{-| Alias for `cons`. Analogous to `::` for lists.
-}
(:::) : a -> LazyList a -> LazyList a
(:::) = cons


infixr 5 +++

{-| Alias for `append`. Analogous to `++` for lists.
-}
(+++) : LazyList a -> LazyList a -> LazyList a
(+++) = append
