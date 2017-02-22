module Lazy.List exposing (..)

{-| Lazy list implementation in Elm.

# Types
@docs LazyList, LazyListView

# Constructors
@docs cons, empty, singleton, iterate, repeat, range, rangeStep, openRange, openRangeStep

# Query operations
@docs isEmpty, head, tail, headAndTail, length, nth, member, all, any, indexOf, indexBy

# Conversions to other containers
@docs toList, fromList, toArray, fromArray

# LazyList transformations
@docs reduce, foldl, foldr, map, zip, append, flatten, concat, scanl

# Common operations
@docs take, takeWhile, drop, dropWhile, cycle, reverse, intersperse, interleave

# Filtering operations
@docs keepIf, filter, dropIf, filterMap, unique

# Chaining operations
@docs andMap, andThen

# Useful math stuff
@docs numbers, naturals, sum, product, maximum, minimum

# All the other maps!
@docs map2, map3, map4, map5

# All the other zips!
@docs zip3, zip4, zip5

# Taking lasy lists apart
@docs partition, unzip

# Special maps
@docs filterMap, concatMap, flattenMap, indexedMap

# All the Cartesian products!
**Warning:** these can get very large if converted to a non-lazy type
or if retained in memory by holding onto the head of the lazy list
and the list is evaluated through its length.
@docs product2, product3, product4, product5

# Sorting operations
@docs sort, sortBy, sortWith

# Infix Operators
@docs (:::), (+++), (!!!)
-}

import Array exposing (Array)
import List
import Maybe exposing (Maybe(..))
import Lazy exposing (Lazy, lazy, force)


-----------
-- TYPES --
-----------


{-| Lazy Lists of a given type, with tails of its own type.
Analogous to `List` type. This is the actual implementation type for the
`LazyList` type. This type is exposed to the user if the user so wishes to
do pattern matching or understand how the list type works. It is not
recommended to work with this type directly. Try working solely with the
provided functions in the package.
This type is necessary to break recursive type aliasing loop.
-}
type LazyListView a
    = Nil
    | Cons a (LazyList a)


{-| Actual Lazy List type.
-}
type alias LazyList a =
    Lazy (LazyListView a)



------------------
-- CONSTRUCTORS --
------------------


{-| Create an empty list.  Analogous to [] for List.
-}
empty : LazyList a
empty =
    lazy <| \() -> Nil


{-| Create a singleton list.  Analogous to List.singleton.

    singleton 1 == cons 1 empty
-}
singleton : a -> LazyList a
singleton a =
    cons a empty


{-| Add a value of the type of the lazy list to the front of a lazy list.

**Warning:**  be careful using `cons' in generating infinite lists
using recursive code, examples as follows for a function producing
all the positive odd numbers:

DO NOT DO THIS:

    numbersOdd : () -> LazyList Int
    numbersOdd() =
      let numbersOddHelper n =
        cons n <| numbersOddHelper (n + 2)
      in numbersOddHelper 1

as it will compile but will fault out with a stack overflow exception.

Do not even do this:

    numbersOdd : () -> LazyList Int
    numbersOdd() =
      let numbersOddHelper n =
        cons n <| lazy <| \() -> force <| numbersOddHelper (n + 2)
      in numbersOddHelper 1

which will run but is about twice as slow due to the
double use of lazy, one use internal to `cons' and one use here.

The preferred way is to use `iterate' as follows:

    numbersOdd : () -> LazyList Int
    numbersOdd() =
      iterate ((+) 2) 1

or in special instances one might use the exposed `LazyListView` constructors
such as `Cons`, which one might do to add upper limit checking as follows:

    numbersOdd : () -> LazyList Int
    numbersOdd() =
      let numbesrOddHelper n =
        let next = n + 2 in
        if next < v then empty else -- check for Int overflow
        lazy <| \() -> Cons v <| numbersOddHelper next
      in numbersOddHelper 1

Both of these last have about the same performance,
with the last just a little slower due to the bounds checking.
-}
cons : a -> LazyList a -> LazyList a
cons a list =
    lazy <| \() -> Cons a list


{-| Create an infinite lazy list of applications of a function on some value.
Equivalent to:

    x ::: f x ::: f (f x) ::: f (f (f x)) ::: ... -- etc...
-}
iterate : (a -> a) -> a -> LazyList a
iterate f a =
    let
        iterateHelper a =
            lazy <|
                \() ->
                    Cons a (iterateHelper (f a))
    in
        iterateHelper a


{-| Repeat a value ad infinitum.  Analogous to List.repeat.
Be careful when you use this. The result of this is a truly infinite list.

Do not try calling `reduce` or `toList` on an infinite list as it'll never
finish computing. Make sure you then filter it down to a finite list with `head`
or `take` or something.
-}
repeat : a -> LazyList a
repeat a =
    lazy <|
        \() ->
            Cons a (repeat a)


{-| Create a lazy list of numbers, every element changing by step.
   You give the lowest and highest numbers that should be in the lazy list,
   as well as the step value; the last value can be lower than the first value,
   in which case the range will be decreasing instead of increasing.
   Examples:

       rangeStep -1 6 2 == -1 ::: 1 ::: 3 ::: 5 ::: empty
       rangeStep 4 4 (anything but 0) == 4 ::: empty
       rangeStep 4 (anything) 0 == same as repeat 4 -> iniinite list
       rangeStep 5 6 -1 == empty, step has wrong sign
       rangeStep 8 7 1 == empty, step has wrong sign
       rangeStep 1 -3 -2 == 1 ::: -1 ::: -3 ::: empty
-}
rangeStep : Int -> Int -> Int -> LazyList Int
rangeStep first last step =
    if last < first && step > 0 then
        empty
    else if last > first && step < 0 then
        empty
    else
        let
            rangeStepHelperPlus n =
                lazy <|
                    \() ->
                        Cons n <|
                            let
                                next =
                                    n + step
                            in
                                if next < n || next > last then
                                    empty
                                else
                                    rangeStepHelperPlus next

            rangeStepHelperMinus n =
                lazy <|
                    \() ->
                        Cons n <|
                            let
                                next =
                                    n + step
                            in
                                if next > n || next < last then
                                    empty
                                else
                                    rangeStepHelperMinus next
        in
            if last < first then
                rangeStepHelperMinus first
            else
                rangeStepHelperPlus first


{-| Create a lazy list of numbers, every element changing by one.
Analogous to List.range except allows for a decreasing range as well.

You give the first and last numbers that should be in the lazy list.
For increasing ranges, this is the same as List.range but for lazy lists;
however, it also permits decreasing ranges where last < first
Examples:

       range -1 3 == -1 ::: 0 ::: 1 ::: 2 ::: 3 ::: empty
       range 4 4 == 4 ::: empty
       range 3 -1 ==  3 ::: 2 ::: 1 ::: 0 ::: -1 ::: empty
-}
range : Int -> Int -> LazyList Int
range first last =
    if first <= last then
        rangeStep first last 1
    else
        rangeStep first last -1


{-| Create an infinite lazy list of numbers, every element changing by step.

You give the first number that should be in the lazy list,
as well as the step value; IF the step value is negative, then
the range will be decreasing else it will be increaseing.
This is like rangeStep but with no end limit.
-}
openRangeStep : Int -> Int -> LazyList Int
openRangeStep first step =
    let
        openRangeStepHelperPlus n =
            lazy <|
                \() ->
                    Cons n <|
                        let
                            next =
                                n + step
                        in
                            if next < n then
                                empty
                            else
                                openRangeStepHelperPlus next

        openRangeStepHelperMinus n =
            lazy <|
                \() ->
                    Cons n <|
                        let
                            next =
                                n + step
                        in
                            if next > n then
                                empty
                            else
                                openRangeStepHelperMinus next
    in
        if step < 0 then
            openRangeStepHelperMinus first
        else
            openRangeStepHelperPlus first


{-| Create an infinite lazy list of numbers, every element increasing by one.

You give the lowest number that should be in the lazy list;
this is like range but with no end limit.
-}
openRange : Int -> LazyList Int
openRange low =
    openRangeStep low 1



----------------------
-- QUERY OPERATIONS --
----------------------


{-| Detect if a lazy list is empty or not.  Analogous to List.isEmpty.
-}
isEmpty : LazyList a -> Bool
isEmpty list =
    case force list of
        Nil ->
            True

        _ ->
            False


{-| Get the head value of a lazy list if it is not empty.
  Analogous to List.head.
-}
head : LazyList a -> Maybe a
head list =
    case force list of
        Nil ->
            Nothing

        Cons first _ ->
            Just first


{-| Get the tail of a lazy list if the list is not empty.
  Analogous to List.tail.
-}
tail : LazyList a -> Maybe (LazyList a)
tail list =
    case force list of
        Nil ->
            Nothing

        Cons _ rest ->
            Just rest


{-| Get the head and tail of a lazy list if the list is not empty.
-}
headAndTail : LazyList a -> Maybe ( a, LazyList a )
headAndTail list =
    case force list of
        Nil ->
            Nothing

        Cons first rest ->
            Just ( first, rest )


{-| Get the length of a lazy list.  Analogous to List.length.

**Warning:**  This will not terminate if the lazy list is infinite.
Its running time is proportional to the length of the lazy list.
-}
length : LazyList a -> Int
length =
    reduce (\_ n -> n + 1) 0


{-| Get the nth element of a lazy list if the list is at least that long;
returns a `Maybe' (`Nothing'/`Just') to indicate there is one or not.

Its running time is proportional to the length of the lazy list.
-}
nth : Int -> LazyList a -> Maybe a
nth n list =
    if n <= 0 then
        case force list of
            Nil ->
                Nothing

            Cons hd _ ->
                Just hd
    else
        case force list of
            Nil ->
                Nothing

            Cons _ tl ->
                nth (n - 1) tl


{-| Test if a value is a member of a lazy list.  Analogous to List.member.

    member 2 (1 ::: 2 ::: 3 ::: empty) == True
    member 0 (1 ::: 2 ::: 3 ::: empty) == False

**Warning:**  This may not terminate if the lazy list is infinite.
and the value is not found.
Its running time is proportional to the length of the lazy list.
-}
member : a -> LazyList a -> Bool
member a list =
    let
        memberHelper list =
            case force list of
                Nil ->
                    False

                Cons first rest ->
                    if first == a then
                        True
                    else
                        memberHelper rest
    in
        memberHelper list


{-| Determine if all elements satisfy the predicate.  Analogous to List.all.

    all isEven (2 ::: 4 ::: empty) == True
    all isEven (2 ::: 3 ::: empty) == False
    all isEven empty == True

**Warning:**  This may not terminate if the lazy list is infinite.
-}
all : (a -> Bool) -> LazyList a -> Bool
all predicate lazylist =
    let
        allHelper lazylist =
            case force lazylist of
                Nil ->
                    True

                Cons first rest ->
                    if predicate first then
                        allHelper rest
                    else
                        False
    in
        allHelper lazylist


{-| Determine if any elements satisfy the predicate.  Analogous to List.any.

    any isEven (2 ::: 3 ::: empty) == True
    any isEven (1 ::: 3 ::: empty) == False
    any isEven empty == False

**Warning:**  This may not terminate if the lazy list is infinite.
-}
any : (a -> Bool) -> LazyList a -> Bool
any predicate lazylist =
    let
        anyHelper lazylist =
            case force lazylist of
                Nil ->
                    False

                Cons first rest ->
                    if predicate first then
                        True
                    else
                        anyHelper rest
    in
        anyHelper lazylist


{-| Finds the zero-based index of the first occurence of the argument in the lazylist,
returning -1 if the search is not successful.

    indexOf 3 (2 ::: 3 ::: 4 :::  empty) == 1
    indexOf 0 (2 ::: 3 ::: 4 :::  empty) == -1
    indexOf 42 empty == -1

**Warning:**  This may not terminate if the lazy list is infinite.
-}
indexOf : a -> LazyList a -> Int
indexOf a lazylist =
    let
        indexOfHelper count lazylist =
            case force lazylist of
                Nil ->
                    -1

                Cons first rest ->
                    if a == first then
                        count
                    else
                        indexOfHelper (count + 1) rest
    in
        indexOfHelper 0 lazylist


{-| Finds the zero-based index of the first occurence of the argument in the lazylist for which
the predicate produces True, returning -1 if the search is not successful.

    indexBy isEven (1 ::: 4 ::: 5 :::  empty) == 1
    indexBy isEven (1 ::: 3 ::: 5 :::  empty) == -1
    indexBy isEven empty == -1

**Warning:**  This may not terminate if the lazy list is infinite.
-}
indexBy : (a -> Bool) -> LazyList a -> Int
indexBy predicate lazylist =
    let
        indexByHelper count lazylist =
            case force lazylist of
                Nil ->
                    -1

                Cons first rest ->
                    if predicate first then
                        count
                    else
                        indexByHelper (count + 1) rest
    in
        indexByHelper 0 lazylist



-----------------------
-- COMMON OPERATIONS --
-----------------------


{-| Take at most `n` many values from a lazy list.  Analogous to List.take.
-}
take : Int -> LazyList a -> LazyList a
take n lazylist =
    if n <= 0 then
        empty
    else
        let
            takeHelper n lazylist =
                if n <= 0 then
                    empty
                else
                    case force lazylist of
                        Nil ->
                            empty

                        Cons first rest ->
                            lazy <|
                                \() ->
                                    Cons first (takeHelper (n - 1) rest)
        in
            lazy <| \() -> force (takeHelper n lazylist)


{-| Take elements from a lazy list as long as the predicate is satisfied.
-}
takeWhile : (a -> Bool) -> LazyList a -> LazyList a
takeWhile predicate lazylist =
    let
        takeWhileHelper lazylist =
            case force lazylist of
                Nil ->
                    empty

                Cons first rest ->
                    if predicate first then
                        lazy <|
                            \() ->
                                Cons first (takeWhileHelper rest)
                    else
                        empty
    in
        lazy <| \() -> force (takeWhileHelper lazylist)


{-| Drop at most `n` many values from a lazy list,
with `drop 0' a no-op.  Analogous to List.crop.
-}
drop : Int -> LazyList a -> LazyList a
drop n lazylist =
    if n <= 0 then
        lazylist
    else
        let
            dropHelper n lazylist =
                if n <= 0 then
                    lazylist
                else
                    case force lazylist of
                        Nil ->
                            empty

                        Cons _ rest ->
                            dropHelper (n - 1) rest
        in
            lazy <| \() -> force (dropHelper n lazylist)


{-| Drop elements from a lazy list as long as the predicate is satisfied.
-}
dropWhile : (a -> Bool) -> LazyList a -> LazyList a
dropWhile predicate lazylist =
    let
        dropWhileHelper lazylist =
            case force lazylist of
                Nil ->
                    empty

                Cons first rest ->
                    if predicate first then
                        dropWhileHelper rest
                    else
                        lazylist
    in
        lazy <| \() -> force (dropWhileHelper lazylist)


{-| Take a lazy list and repeat it ad infinitum. This cycles a finite list
by putting the front after the end of the list. This results in a no-op in
the case of an infinite list.

    cycle of a 1,2,3 list produces 1,2,3,1,2,3,...
-}
cycle : LazyList a -> LazyList a
cycle lazylist =
    lazylist
        +++ (lazy <|
                \() ->
                    force (cycle lazylist)
            )


{-| Reverse a lazy list.  Analogous to List.reverse.

**Warning:**  This will not terminate if the lazy list is infinite.
-}
reverse : LazyList a -> LazyList a
reverse =
    reduce cons empty


{-| Places the given value between all members of the given lazy list.
  Analogous to List.intersperse.

    instersperse 42 in the list of 1,2,3 produces 1,42,2,42,3.
-}
intersperse : a -> LazyList a -> LazyList a
intersperse a lazylist =
    let
        intersperseHelper lazylist =
            case force lazylist of
                Nil ->
                    empty

                Cons first rest ->
                    lazy <|
                        \() ->
                            Cons a <|
                                lazy <|
                                    \() ->
                                        Cons first (intersperseHelper rest)
    in
        lazy <|
            \() ->
                case force lazylist of
                    Nil ->
                        Nil

                    Cons first rest ->
                        Cons first (intersperseHelper rest)


{-| Interleave the elements of a lazy list in another lazy list.
if one list is shorter than the other then the
longer list just continues un-interleaved.
For instace:  interleave the lists of 1,2,3 with the list 4,5,6,7,8
produces the list 1,3,2,5,3,6,7,8
-}
interleave : LazyList a -> LazyList a -> LazyList a
interleave lazylist1 lazylist2 =
    let
        interleaveHelper lazylist1 lazylist2 =
            case force lazylist1 of
                Nil ->
                    lazylist2

                Cons first1 rest1 ->
                    case force lazylist2 of
                        Nil ->
                            lazylist1

                        Cons first2 rest2 ->
                            lazy <|
                                \() ->
                                    Cons first1 <|
                                        lazy <|
                                            \() ->
                                                Cons first2 (interleaveHelper rest1 rest2)
    in
        lazy <| \() -> force (interleaveHelper lazylist1 lazylist2)



--------------------------
-- FILTERING OPERATIONS --
--------------------------


{-| Keep all elements in a lazy list that satisfy the given predicate.
-}
keepIf : (a -> Bool) -> LazyList a -> LazyList a
keepIf predicate lazylist =
    let
        keepIfHelper lazylist =
            case force lazylist of
                Nil ->
                    empty

                Cons first rest ->
                    if predicate first then
                        lazy <|
                            \() ->
                                Cons first (keepIfHelper rest)
                    else
                        keepIfHelper rest
    in
        lazy <| \() -> force (keepIfHelper lazylist)


{-| Alias of keepIf; analogous to List.filter.
-}
filter : (a -> Bool) -> LazyList a -> LazyList a
filter =
    keepIf


{-| Drop all elements in a lazy list that satisfy the given predicate.
-}
dropIf : (a -> Bool) -> LazyList a -> LazyList a
dropIf predicate lazylist =
    let
        dropIfHelper lazylist =
            case force lazylist of
                Nil ->
                    empty

                Cons first rest ->
                    if predicate first then
                        dropIfHelper rest
                    else
                        lazy <|
                            \() ->
                                Cons first (dropIfHelper rest)
    in
        lazy <| \() -> force (dropIfHelper lazylist)


{-| Remove all duplicates from a lazy list ("dedups") and
return a lazy list of distinct elements.

**Warning:**  This will not terminate if the lazy list is infinite.
Its running time is proportional to the factorial of the length of the lazy list,
so can take a very long time for long lazy lists.
-}
unique : LazyList a -> LazyList a
unique lazylist =
    let
        uniqueHelper lazylist =
            case force lazylist of
                Nil ->
                    empty

                Cons first rest ->
                    if member first rest then
                        unique rest
                    else
                        lazy <|
                            \() ->
                                Cons first (unique rest)
    in
        lazy <| \() -> force (uniqueHelper lazylist)



------------------------------
-- LAZYLIST TRANSFORMATIONS --
------------------------------


{-| Reduce a lazy list with a given reducer and an initial value.
Example :

    reduce (+) 0 (1 ::: 2 ::: 3 ::: 4 ::: empty) == 10

**Warning:**  This will not terminate if the lazy list is infinite.
-}
reduce : (a -> b -> b) -> b -> LazyList a -> b
reduce reducer b lazylist =
    let
        reduceHelper b lazylist =
            case force lazylist of
                Nil ->
                    b

                Cons first rest ->
                    reduceHelper (reducer first b) rest
    in
        reduceHelper b lazylist


{-| Analogous to `List.foldl`. Is an alias for `reduce`.

**Warning:**  This will not terminate if the lazy list is infinite.
-}
foldl : (a -> b -> b) -> b -> LazyList a -> b
foldl =
    reduce


{-| Analogous to `List.foldr`;
Fuunctionally the same as `reduce'/`foldl` but starts from
the right end of the list rather than the left.

**Warning:**  This will not terminate if the lazy list is infinite.
-}
foldr : (a -> b -> b) -> b -> LazyList a -> b
foldr reducer b lazylist =
    let
        rlist alist lazylist =
            case force lazylist of
                Nil ->
                    alist

                Cons first rest ->
                    rlist (first :: alist) rest
    in
        lazylist |> rlist [] |> List.foldl reducer b


{-| Append a lazy list to another lazy list.
Example:

    (1 ::: 2 ::: 3 ::: empty) +++ (4 ::: 5 ::: 6 ::: empty)

produces the list of 1, 2, 3, 4, 5, and 6.

Note that it does effectively nothing of the first lazy list is infinite.
-}
append : LazyList a -> LazyList a -> LazyList a
append lazylist1 lazylist2 =
    let
        appendHelper lazylist1 =
            case force lazylist1 of
                Nil ->
                    lazylist2

                Cons first rest ->
                    lazy <|
                        \() ->
                            Cons first (appendHelper rest)
    in
        lazy <| \() -> force (appendHelper lazylist1)


{-| Flatten a lazy list of lazy lists into a single lazy list by
appending all the inner lists into one big list.
Example:

    flatten ((1 ::: 2 ::: 3 ::: empty) ::: (4 ::: 5 ::: 6 ::: empty) ::: empty)

produces the flat list of 1, 2, 3, 4, 5, and 6.

Note that if any of the lazy lists are infinite, then it cannot get past the first of these.
-}
flatten : LazyList (LazyList a) -> LazyList a
flatten lazylist =
    let
        flattenHelper lazylist =
            case force lazylist of
                Nil ->
                    empty

                Cons first rest ->
                    lazy <|
                        \() ->
                            force (first +++ flattenHelper rest)
    in
        lazy <| \() -> force (flattenHelper lazylist)


{-| Alias of flatten; analogous to List.concat.
-}
concat : LazyList (LazyList a) -> LazyList a
concat =
    flatten


{-| Reduce a lazy list from the left, building up all of the intermediate results into a lazy list.
  Analogous to List.scanl.

    scanl (+) 0 (1 ::: 2 ::: 3 ::: 4 ::: empty) == (0 ::: 1 ::: 3 ::: 6 ::: 10 ::: empty)
-}
scanl : (a -> b -> b) -> b -> LazyList a -> LazyList b
scanl arity2func initial lazylist =
    let
        scanlHelper accumulated lazylist =
            case force lazylist of
                Nil ->
                    lazy <|
                        \() ->
                            Cons accumulated empty

                Cons first rest ->
                    lazy <|
                        \() ->
                            Cons accumulated <|
                                scanlHelper (arity2func first accumulated) rest
    in
        lazy <| \() -> force (scanlHelper initial lazylist)



-----------------------
-- USEFUL MATH STUFF --
-----------------------


{-| The infinite lazy list of counting numbers.
i.e.:
    1 ::: 2 ::: 3 ::: 4 ::: 5 ::: ... -- etc...
-}
numbers : () -> LazyList number
numbers () =
    let
        numbersHelper n =
            if n < 0 then
                empty
            else
                -- check for Int overflow
                lazy <| \() -> Cons n <| numbersHelper (n + 1)
    in
        numbersHelper 1


{-| The infinite lazy list of natural numbers.
i.e.:
    0 ::: 1 ::: 2 ::: 3 ::: 4 ::: 5 ::: ... -- etc...
-}
naturals : () -> LazyList number
naturals () =
    0 ::: numbers ()


{-| Get the sum of a lazy list of numbers.

**Warning:** This will not terminate if the list is infinite.
Its running time is proportional to the length of the lazy list.
-}
sum : LazyList number -> number
sum =
    reduce (+) 0


{-| Get the product of a lazy list of numbers.

**Warning:**  This will not terminate if the lazy list is infinite.
Its running time is proportional to the length of the lazy list.
-}
product : LazyList number -> number
product =
    reduce (*) 1


{-| Find the maximum element in a non-empty lazy list.  Analogous to List.maximum.

    maximum (1 ::: 4 ::: 2 ::: empty) == Just 4
    maximum empty                     == Nothing

**Warning:**  This will not terminate if the lazy list is infinite.
Its running time is proportional to the length of the lazy list.
-}
maximum : LazyList comparable -> Maybe comparable
maximum lazylist =
    let
        maximumHelper accumulated lazylist =
            case force lazylist of
                Nil ->
                    accumulated

                Cons first rest ->
                    case accumulated of
                        Nothing ->
                            maximumHelper (Just first) rest

                        Just v ->
                            maximumHelper
                                (Just
                                    (if first > v then
                                        first
                                     else
                                        v
                                    )
                                )
                                rest
    in
        maximumHelper Nothing lazylist


{-| Find the minimum element in a non-empty lazy list.  Analogous to List.minimum.

    maximum (3 ::: 1 ::: 2 ::: empty) == Just 1
    maximum empty                     == Nothing

**Warning:**  This will not terminate if the lazy list is infinite.
Its running time is proportional to the length of the lazy list.
-}
minimum : LazyList comparable -> Maybe comparable
minimum lazylist =
    let
        minimumHelper accumulated lazylist =
            case force lazylist of
                Nil ->
                    accumulated

                Cons first rest ->
                    case accumulated of
                        Nothing ->
                            minimumHelper (Just first) rest

                        Just v ->
                            minimumHelper
                                (Just
                                    (if first < v then
                                        first
                                     else
                                        v
                                    )
                                )
                                rest
    in
        minimumHelper Nothing lazylist



------------------
-- ALL THE MAPS --
------------------


{-| Map a function to a lazy list;
the output ends when the input list ends.
Example:

    numbers() |> map ((*) 2)

produces a lazy list of all the positive multiples of two.
-}
map : (a -> b) -> LazyList a -> LazyList b
map f lazylist =
    let
        mapHelper lazylist =
            case force lazylist of
                Nil ->
                    empty

                Cons first rest ->
                    lazy <|
                        \() ->
                            Cons (f first) (mapHelper rest)
    in
        lazy <| \() -> force (mapHelper lazylist)


{-| Map a function to two llazy ists;
the output ends when either of the input lists end.
-}
map2 : (a -> b -> c) -> LazyList a -> LazyList b -> LazyList c
map2 f lazylist1 lazylist2 =
    let
        map2Helper lazylist1 lazylist2 =
            case force lazylist1 of
                Nil ->
                    empty

                Cons first1 rest1 ->
                    case force lazylist2 of
                        Nil ->
                            empty

                        Cons first2 rest2 ->
                            lazy <|
                                \() ->
                                    Cons (f first1 first2) (map2Helper rest1 rest2)
    in
        lazy <| \() -> force (map2Helper lazylist1 lazylist2)


{-| Map a function to three lazy lists;
the output ends when any of the input lists end.
-}
map3 : (a -> b -> c -> d) -> LazyList a -> LazyList b -> LazyList c -> LazyList d
map3 f lazylist1 lazylist2 lazylist3 =
    let
        map3Helper lazylist1 lazylist2 lazylist3 =
            case force lazylist1 of
                Nil ->
                    empty

                Cons first1 rest1 ->
                    case force lazylist2 of
                        Nil ->
                            empty

                        Cons first2 rest2 ->
                            case force lazylist3 of
                                Nil ->
                                    empty

                                Cons first3 rest3 ->
                                    lazy <|
                                        \() ->
                                            Cons (f first1 first2 first3) (map3Helper rest1 rest2 rest3)
    in
        lazy <| \() -> force (map3Helper lazylist1 lazylist2 lazylist3)


{-| Map a function to four lazy lists;
the output ends when any of the input lists end.
-}
map4 : (a -> b -> c -> d -> e) -> LazyList a -> LazyList b -> LazyList c -> LazyList d -> LazyList e
map4 f lazylist1 lazylist2 lazylist3 lazylist4 =
    let
        map4Helper lazylist1 lazylist2 lazylist3 lazylist4 =
            case force lazylist1 of
                Nil ->
                    empty

                Cons first1 rest1 ->
                    case force lazylist2 of
                        Nil ->
                            empty

                        Cons first2 rest2 ->
                            case force lazylist3 of
                                Nil ->
                                    empty

                                Cons first3 rest3 ->
                                    case force lazylist4 of
                                        Nil ->
                                            empty

                                        Cons first4 rest4 ->
                                            lazy <|
                                                \() ->
                                                    Cons (f first1 first2 first3 first4)
                                                        (map4Helper rest1 rest2 rest3 rest4)
    in
        lazy <| \() -> force (map4Helper lazylist1 lazylist2 lazylist3 lazylist4)


{-| Map a function to five lazylists;
the output ends when any of the input lists end.
-}
map5 : (a -> b -> c -> d -> e -> f) -> LazyList a -> LazyList b -> LazyList c -> LazyList d -> LazyList e -> LazyList f
map5 f lazylist1 lazylist2 lazylist3 lazylist4 lazylist5 =
    let
        map5Helper lazylist1 lazylist2 lazylist3 lazylist4 lazylist5 =
            case force lazylist1 of
                Nil ->
                    empty

                Cons first1 rest1 ->
                    case force lazylist2 of
                        Nil ->
                            empty

                        Cons first2 rest2 ->
                            case force lazylist3 of
                                Nil ->
                                    empty

                                Cons first3 rest3 ->
                                    case force lazylist4 of
                                        Nil ->
                                            empty

                                        Cons first4 rest4 ->
                                            case force lazylist5 of
                                                Nil ->
                                                    empty

                                                Cons first5 rest5 ->
                                                    lazy <|
                                                        \() ->
                                                            Cons (f first1 first2 first3 first4 first5)
                                                                (map5Helper rest1 rest2 rest3 rest4 rest5)
    in
        lazy <| \() -> force (map5Helper lazylist1 lazylist2 lazylist3 lazylist4 lazylist5)



-------------------------
-- CHAINING OPERATIONS --
-------------------------


{-| Known as `mapN` in some circles. Allows you to apply `map` in cases
where then number of arguments are greater than 5.
The argument order is such that it works well with `|>` chains.
-}
andMap : LazyList a -> LazyList (a -> b) -> LazyList b
andMap listVal listFuncs =
    map2 (<|) listFuncs listVal


{-| Chain lazy list producing operations. Map then flatten.
-}
andThen : (a -> LazyList b) -> LazyList a -> LazyList b
andThen f lazylist =
    map f lazylist |> flatten



------------------
-- ALL THE ZIPS --
------------------


{-| zip two lazy lists into a lazy list of a pair tuple,
each tuple containing the matching nth elements of the lists;
with the list of tuplies ending when either of the lists ends..
Example:

    zip (1 ::: 2 ::: 3 ::: empty) (4 ::: 5 ::: 6 ::: 7 ::: empty)

produces:  ((1, 4) ::: (2, 5) ::: (3, 6) ::: empty) -- 7 not used
-}
zip : LazyList a -> LazyList b -> LazyList ( a, b )
zip =
    map2 (,)


{-| zip three lazy lists into a lazy list of a triplet tuple,
each tuple containing the matching nth elements of the lists;
with the list of tuplies ending when any of the lists ends..
-}
zip3 : LazyList a -> LazyList b -> LazyList c -> LazyList ( a, b, c )
zip3 =
    map3 (,,)


{-| zip four lazy lists into a lazy list of a four-tuple,
each tuple containing the matching nth elements of the lists;
with the list of tuplies ending when any of the lists ends..
-}
zip4 : LazyList a -> LazyList b -> LazyList c -> LazyList d -> LazyList ( a, b, c, d )
zip4 =
    map4 (,,,)


{-| zip five lazy lists into a lazy list of a five-tuple,
each tuple containing the matching nth elements of the lists;
with the list of tuplies ending when any of the lists ends..
-}
zip5 : LazyList a -> LazyList b -> LazyList c -> LazyList d -> LazyList e -> LazyList ( a, b, c, d, e )
zip5 =
    map5 (,,,,)



-----------------------------
-- TAKING LAZY LISTS APART --
-----------------------------


{-| Partition a lazy list based on a predicate. producing a tuple of two lazy lists
with the first lazy list containing all values that satisfy the predicate,
and the second lazy list containing all the value that do not;
  Analogous to List.partition.
Examples:

    partition (\x -> x < 2) (0 ::: 1 ::: 2 ::: empty) == ((0 ::: 1 ::: empty), (2 ::: empty))
    partition isEven        (0 ::: 1 ::: 2 ::: empty) == (0 ::: 2 ::: empty, 1 ::: empty)

Caution:  the resulting lazy lists may be consumed at different rates, meaning that the
source lazy list may not be garbage collected.  For instance, if one takes the nth value of
the first lazy list but doesn't use the second, the entire source lazy list must be
retained in memory for potential use by the second lazy list if there is
a reference to the second held in any function
-}
partition : (a -> Bool) -> LazyList a -> ( LazyList a, LazyList a )
partition predicate lazylist =
    let
        partitionHelper1 lazylist =
            case force lazylist of
                Nil ->
                    empty

                Cons v rest ->
                    if predicate v then
                        lazy <|
                            \() ->
                                Cons v (partitionHelper1 rest)
                    else
                        partitionHelper1 rest

        partitionHelper2 lazylist =
            case force lazylist of
                Nil ->
                    empty

                Cons v rest ->
                    if predicate v then
                        partitionHelper2 rest
                    else
                        lazy <|
                            \() ->
                                Cons v (partitionHelper2 rest)
    in
        ( lazy <| \() -> force (partitionHelper1 lazylist)
        , lazy <| \() -> force (partitionHelper2 lazylist)
        )


{-| Decompose a lazy list of tuples into a tuple of lazy lists;
  Analogous to List.unzip.
Example:

    unzip ((0, True) ::: (17, False) ::: (1337, True) ::: empty) ==
      ((0 ::: 17 ::: 1337 ::: empty), (True ::: False ::: True ::: empty))

Caution:  the resulting lazy lists may be consumed at different rates, meaning that the
source lazy list may not be garbage collected.  For instance, if one takes the nth value of
the first lazy list but doesn't use the second, the entire source lazy list must be
retained in memory for potential use by the second lazy list if there is
a reference to the second held in any function
-}
unzip : LazyList ( a, b ) -> ( LazyList a, LazyList b )
unzip lazylist =
    let
        unzipHelper1 lazylist =
            case force lazylist of
                Nil ->
                    empty

                Cons ( v, _ ) rest ->
                    lazy <| \() -> Cons v (unzipHelper1 rest)

        unzipHelper2 lazylist =
            case force lazylist of
                Nil ->
                    empty

                Cons ( _, v ) rest ->
                    lazy <| \() -> Cons v (unzipHelper2 rest)
    in
        ( lazy <| \() -> force (unzipHelper1 lazylist)
        , lazy <| \() -> force (unzipHelper2 lazylist)
        )



-----------------------------
-- SPECIAL MAPS --
-----------------------------


{-| Map a function that may fail over a lazy list, keeping only
the values that were successfully transformed.  Analogous to List.filterMap.
Examples:

    isTeen : Int -> Maybe Int
    isTeen n =
      if 13 <= n && n <= 19 then
        Just n

      else
        Nothing

    onlyTeens =
      filterMap isTeen (3 ::: 15 ::: 12 ::: 18 ::: 24 ::: empty) == (15 ::: 18 ::: empty)
-}
filterMap : (a -> Maybe b) -> LazyList a -> LazyList b
filterMap transform lazylist =
    let
        filterMapHelper lazylist =
            case force lazylist of
                Nil ->
                    empty

                Cons first rest ->
                    case transform first of
                        Just val ->
                            lazy <|
                                \() ->
                                    Cons val (filterMapHelper rest)

                        Nothing ->
                            filterMapHelper rest
    in
        lazy <| \() -> force (filterMapHelper lazylist)


{-| Alias of flattenMap.
-}
concatMap : (a -> LazyList b) -> LazyList a -> LazyList b
concatMap f =
    flatten << map f


{-| Map a given function onto a lazy list and flatten the resulting lazy lists.
  Alias of concatMap.

    flattenMap f xs == flatten (map f xs)
-}
flattenMap : (a -> LazyList b) -> LazyList a -> LazyList b
flattenMap f =
    flatten << map f


{-| Same as map but the function is also applied to the index of each element (starting at zero);
  Analogous to List.indexedMap.

    indexedMap (,) ("Tom" ::: "Sue" ::: "Bob" ::: empty) == ( (0,"Tom") ::: (1,"Sue") ::: (2,"Bob") ::: empty )

-}
indexedMap : (Int -> a -> b) -> LazyList a -> LazyList b
indexedMap indexedfunc lazylist =
    let
        indexedMapHelper lazylist =
            case force lazylist of
                Nil ->
                    empty

                Cons ( i, v ) rest ->
                    lazy <|
                        \() ->
                            Cons (indexedfunc i v) (indexedMapHelper rest)
    in
        lazy <| \() -> force (indexedMapHelper (zip (naturals ()) lazylist))



--------------------------------
-- ALL THE CARTESIAN PRODUCTS --
--------------------------------


{-| Create a lazy list containing all possible pairs in the given two lazy lists.
Example:

    product2 (1 ::: 2 ::: empty) (3 ::: 4 ::: empty)

produces: ((1, 3) ::: (1, 4) ::: (2, 3) ::: (2, 4) ::: empty)

**Caution:**  this only works if the second lazy list is of finite length.
-}
product2 : LazyList a -> LazyList b -> LazyList ( a, b )
product2 lazylist1 lazylist2 =
    let
        product2Helper lazylist1 =
            case force lazylist1 of
                Nil ->
                    empty

                Cons first1 rest1 ->
                    case force lazylist2 of
                        Nil ->
                            empty

                        _ ->
                            map ((,) first1) lazylist2 +++ product2Helper rest1
    in
        lazy <| \() -> force (product2Helper lazylist1)


{-| Create a lazy list containing all possible triple tuples in the given three lazy lists.

**Caution:**  this only works if the lazy lists after the first are of finite length.
-}
product3 : LazyList a -> LazyList b -> LazyList c -> LazyList ( a, b, c )
product3 lazylist1 lazylist2 lazylist3 =
    let
        product3Helper lazylist1 =
            case force lazylist1 of
                Nil ->
                    empty

                Cons first1 rest1 ->
                    case force lazylist2 of
                        Nil ->
                            empty

                        _ ->
                            map (\( b, c ) -> ( first1, b, c ))
                                (product2 lazylist2 lazylist3)
                                +++ product3Helper rest1
    in
        lazy <| \() -> force (product3Helper lazylist1)


{-| Create a lazy list containing all possible four-tuples in the given four lazy lists.

**Caution:**  this only works if the lazy lists after the first are of finite length.
-}
product4 : LazyList a -> LazyList b -> LazyList c -> LazyList d -> LazyList ( a, b, c, d )
product4 lazylist1 lazylist2 lazylist3 lazylist4 =
    let
        product4Helper lazylist1 =
            case force lazylist1 of
                Nil ->
                    empty

                Cons first1 rest1 ->
                    case force lazylist2 of
                        Nil ->
                            empty

                        _ ->
                            map (\( b, c, d ) -> ( first1, b, c, d ))
                                (product3 lazylist2 lazylist3 lazylist4)
                                +++ product4Helper rest1
    in
        lazy <| \() -> force (product4Helper lazylist1)


{-| Create a lazy list containing all possible five-tuples in the given five lazy lists.

**Caution:**  this only works if the lazy lists after the first are of finite length.
-}
product5 : LazyList a -> LazyList b -> LazyList c -> LazyList d -> LazyList e -> LazyList ( a, b, c, d, e )
product5 lazylist1 lazylist2 lazylist3 lazylist4 lazylist5 =
    let
        product5Helper lazylist1 =
            case force lazylist1 of
                Nil ->
                    empty

                Cons first1 rest1 ->
                    case force lazylist2 of
                        Nil ->
                            empty

                        _ ->
                            map (\( b, c, d, e ) -> ( first1, b, c, d, e ))
                                (product4 lazylist2 lazylist3 lazylist4 lazylist5)
                                +++ product5Helper rest1
    in
        lazy <| \() -> force (product5Helper lazylist1)



------------------------------------------
-- TRANSFORMATIONS TO OTHER CONTAINEERS --
------------------------------------------


{-| Convert a lazy list to a normal list.

**Warning:**  This will not terminate if the lazy list is infinite;
it will also eat up all available memory.
-}
toList : LazyList a -> List a
toList lazylist =
    let
        toListHelper lazylist list =
            case force lazylist of
                Nil ->
                    List.reverse list

                Cons first rest ->
                    toListHelper rest (first :: list)
    in
        toListHelper lazylist []


{-| Convert a normal list to a lazy list.
-}
fromList : List a -> LazyList a
fromList lazylist =
    let
        fromListHelper lazylist =
            case lazylist of
                [] ->
                    empty

                head :: rest ->
                    lazy <|
                        \() ->
                            Cons head (fromListHelper rest)
    in
        lazy <| \() -> force (fromListHelper lazylist)


{-| Convert a lazy list to an array.

**Warning:**  This will not terminate if the lazy list is infinite
it will also eat up all available memory.
-}
toArray : LazyList a -> Array a
toArray lazylist =
    let
        toArrayHelper lazylist array =
            case force lazylist of
                Nil ->
                    array

                Cons first rest ->
                    toArrayHelper rest (Array.push first array)
    in
        toArrayHelper lazylist Array.empty


{-| Convert an array to a lazy list.
-}
fromArray : Array a -> LazyList a
fromArray array =
    let
        arrayLength =
            Array.length array

        fromArrayHelper i =
            if i >= arrayLength then
                empty
            else
                case Array.get i array of
                    Nothing ->
                        empty

                    Just v ->
                        lazy <|
                            \() ->
                                Cons v (fromArrayHelper (i + 1))
    in
        lazy <| \() -> force (fromArrayHelper 0)



-----------------------
-- SORTING OPERATORS --
-----------------------


{-| Sort values from lowest to highest.  Analogous to List.sort.

    sort (3 ::: 1 ::: 5 ::: empty) == (1 ::: 3 ::: 5 ::: empty)

**Warning:**  This will not terminate if the lazy list is infinite.
-}
sort : LazyList comparable -> LazyList comparable
sort lazylist =
    lazylist |> toList |> List.sort |> fromList


{-| Sort values by a derived property.  Analogous to List.sortBy.

    alice = { name = "Alice", height = 1.62 }
    bob   = { name = "Bob"  , height = 1.85 }
    chuck = { name = "Chuck", height = 1.76 }

    sortBy .name   (chuck ::: alice ::: bob ::: empty) == (alice ::: bob ::: chunk ::: empty)
    sortBy .height (chuck ::: alice ::: bob ::: empty) == (alice ::: chuck ::: bob ::: empty)

    sortBy String.length ("mouse" ::: "cat" ::: empty) == ("cat" ::: "mouse" ::: empty)

**Warning:**  This will not terminate if the lazy list is infinite.
-}
sortBy : (a -> comparable) -> LazyList a -> LazyList a
sortBy sortbyfunc lazylist =
    lazylist |> toList |> List.sortBy sortbyfunc |> fromList


{-| Sort values with a custom comparison function.  Analogous to List.sortWith.

    flippedComparison a b =
        case compare a b of
          LT -> GT
          EQ -> EQ
          GT -> LT

    sortWith flippedComparison (1 ::: 2 ::: 3 ::: 4 ::: 5 ::: empty) == (5 ::: 4 ::: 3 ::: 2 ::: 1 ::: empty)

This is also the most general sort function, allowing you to define any other: sort == sortWith compare

**Warning:**  This will not terminate if the lazy list is infinite.
-}
sortWith : (a -> a -> Order) -> LazyList a -> LazyList a
sortWith sortwithfunc lazylist =
    lazylist |> toList |> List.sortWith sortwithfunc |> fromList



---------------------
-- INFIX OPERATORS --
---------------------


infixr 5 :::


{-| Alias for `cons`. Analogous to `::` for lists.
Use:  1 ::: 2 ::: empty
must always terminate with `empty' or a recursive function that
produces a `LazyList' of the same type as other elements.
-}
(:::) : a -> LazyList a -> LazyList a
(:::) =
    cons


infixr 5 +++


{-| Alias for `append`. Analogous to `++` for lists.
-}
(+++) : LazyList a -> LazyList a -> LazyList a
(+++) =
    append


infixl 5 !!!


{-| Alias for `nth`.
Takes time proportional to the length of the lasy list.
-}
(!!!) : LazyList a -> Int -> Maybe a
(!!!) lazylist n =
    nth n lazylist

