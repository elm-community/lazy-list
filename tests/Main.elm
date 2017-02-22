import Html exposing (Html, Attribute, div, h1, h2, h3, h4, td, tr, text, table, thead, th)
import Html.Attributes exposing (attribute, style, align)
import List

import Lazy exposing (Lazy, lazy, force)
import Lazy.List exposing (..)

import Bitwise as Bw
import Tuple
import Array
import Random

import Native.DateNow

type alias TestText = String

type alias TestFunction = () -> Answer

type alias Expected = String

type alias Answer = String

type Test = Test TestText TestFunction Expected

lazyListToString : LazyList a -> String
lazyListToString lazylist =
  let lazyListToStringHelper acc count lazylist =
    if count >= 5 then acc ++ "... ]]]" else
    case force lazylist of
      Nil -> acc ++ "Empty ]]]"
      Cons first rest ->
        lazyListToStringHelper (acc ++ toString first ++ ", ") (count + 1) rest
  in lazyListToStringHelper "[[[ " 0 lazylist

criticals : List Test
criticals =
  [
    Test "empty" (\() -> empty |> lazyListToString) "[[[ Empty ]]]"
  , Test "cons 1 empty" (\() -> cons 1 empty |> lazyListToString) "[[[ 1, Empty ]]]"
  , Test "iterate ((+) 1) 1" (\() -> iterate ((+) 1) 1 |> lazyListToString) "[[[ 1, 2, 3, 4, 5, ... ]]]"
  , Test "repeat 7" (\() -> repeat 7 |> lazyListToString) "[[[ 7, 7, 7, 7, 7, ... ]]]"
  , Test "range -1 2" (\() -> range -1 2 |> lazyListToString) "[[[ -1, 0, 1, 2, Empty ]]]"
  , Test "range 2 -1" (\() -> range 2 -1 |> lazyListToString) "[[[ 2, 1, 0, -1, Empty ]]]"
  , Test "rangeStep -2 5 2" (\() -> rangeStep -2 5 2 |> lazyListToString) "[[[ -2, 0, 2, 4, Empty ]]]"
  , Test "rangeStep -1 5 2" (\() -> rangeStep -1 5 2 |> lazyListToString) "[[[ -1, 1, 3, 5, Empty ]]]"
  , Test "rangeStep -2 5 -1" (\() -> rangeStep -2 5 -1 |> lazyListToString) "[[[ Empty ]]]"
  , Test "rangeStep 3 5 0" (\() -> rangeStep 3 5 0 |> lazyListToString) "[[[ 3, 3, 3, 3, 3, ... ]]]"
  , Test "rangeStep 1 -6 1" (\() -> rangeStep 1 -6 1 |> lazyListToString) "[[[ Empty ]]]"
  , Test "rangeStep 1 -6 0" (\() -> rangeStep 1 -6 0 |> lazyListToString) "[[[ 1, 1, 1, 1, 1, ... ]]]"
  , Test "rangeStep 5 -2 -2" (\() -> rangeStep 5 -2 -2 |> lazyListToString) "[[[ 5, 3, 1, -1, Empty ]]]"
  , Test "rangeStep 4 -2 -2" (\() -> rangeStep 4 -2 -2 |> lazyListToString) "[[[ 4, 2, 0, -2, Empty ]]]"
  , Test "openRange -2" (\() -> openRange -2 |> lazyListToString) "[[[ -2, -1, 0, 1, 2, ... ]]]"
  , Test "openRangeStep -2 2" (\() -> openRangeStep -2 2 |> lazyListToString) "[[[ -2, 0, 2, 4, 6, ... ]]]"
  , Test "openRangeStep 3 0" (\() -> openRangeStep 3 0 |> lazyListToString) "[[[ 3, 3, 3, 3, 3, ... ]]]"
  , Test "openRangeStep 5 -2" (\() -> openRangeStep 5 -2 |> lazyListToString) "[[[ 5, 3, 1, -1, -3, ... ]]]"
  , Test "length (range 1 4)" (\() -> length (range 1 4) |> toString) "4"
  , Test "length empty" (\() -> length empty |> toString) "0"
  , Test "nth 2 (range 1 4)" (\() -> nth 2 (range 1 4) |> toString) "Just 3"
  , Test "nth -1 (range 1 4)" (\() -> nth -1 (range 1 4) |> toString) "Just 1"
  , Test "nth 0 (range 1 4)" (\() -> nth 0 (range 1 4) |> toString) "Just 1"
  , Test "nth 4 (range 1 4)" (\() -> nth 4 (range 1 4) |> toString) "Nothing"
  , Test "nth 4 empty" (\() -> nth 4 empty |> toString) "Nothing"
  , Test "numbers()" (\() -> numbers() |> lazyListToString) "[[[ 1, 2, 3, 4, 5, ... ]]]"
  , Test "naturals()" (\() -> naturals() |> lazyListToString) "[[[ 0, 1, 2, 3, 4, ... ]]]"
  , Test "range 1 4 |> takeWhile ((>) 3)" (\() -> range 1 4 |> takeWhile ((>) 3) |> lazyListToString) "[[[ 1, 2, Empty ]]]"
  , Test "range 1 4 |> takeWhile ((>) 42)" (\() -> range 1 4 |> takeWhile ((>) 42) |> lazyListToString) "[[[ 1, 2, 3, 4, Empty ]]]"
  , Test "numbers() |> takeWhile ((>) 4)" (\() -> numbers() |> takeWhile ((>) 4) |> lazyListToString) "[[[ 1, 2, 3, Empty ]]]"
  , Test "numbers() |> takeWhile ((>) 42)" (\() -> numbers() |> takeWhile ((>) 42) |> lazyListToString) "[[[ 1, 2, 3, 4, 5, ... ]]]"
  , Test "rangeStep 1 5 2 |> map ((*) 2)" (\() -> rangeStep 1 5 2 |> map ((*) 2) |> lazyListToString) "[[[ 2, 6, 10, Empty ]]]"
  ]

isEven : Int -> Bool
isEven n = (Bw.and n 1) == 0

isTeen : Int -> Maybe Int
isTeen n =
  if 13 <= n && n <= 19 then Just n
  else Nothing

ordinarys : List Test
ordinarys =
  [
    Test "singleton 42" (\() -> singleton 42 |> lazyListToString) "[[[ 42, Empty ]]]"
  , Test "isEmpty empty" (\() -> isEmpty empty |> toString) "True"
  , Test "isEmpty (cons 1 empty)" (\() -> isEmpty (cons 1 empty) |> toString) "False"
  , Test "head empty" (\() -> head empty |> toString) "Nothing"
  , Test "head (cons 1 empty)" (\() -> head (cons 1 empty) |> toString) "Just 1"
  , Test "tail empty" (\() -> tail empty |> toString) "Nothing"
  , Test "tail (cons 1 empty)" (\() -> tail (cons 1 empty) |> toString) "Just (Lazy <function>)"
  , Test "headAndTail empty" (\() -> headAndTail empty |> toString) "Nothing"
  , Test "headAndTail (cons 1 empty)" (\() -> headAndTail (cons 1 empty) |> toString) "Just (1,Lazy <function>)"
  , Test "member 2 (1 ::: 2 ::: 3 ::: empty)" (\() -> member 2 (1 ::: 2 ::: 3 ::: empty) |> toString) "True"
  , Test "member 0 (1 ::: 2 ::: 3 ::: empty)" (\() -> member 0 (1 ::: 2 ::: 3 ::: empty) |> toString) "False"
  , Test "all isEven (2 ::: 4 ::: empty)" (\() -> all isEven (2 ::: 4 ::: empty) |> toString) "True"
  , Test "all isEven (2 ::: 3 ::: empty)" (\() -> all isEven (2 ::: 3 ::: empty) |> toString) "False"
  , Test "all isEven empty" (\() -> all isEven empty |> toString) "True"
  , Test "any isEven (2 ::: 3 ::: empty)" (\() -> any isEven (2 ::: 3 ::: empty) |> toString) "True"
  , Test "any isEven (1 ::: 3 ::: empty)" (\() -> any isEven (1 ::: 3 ::: empty) |> toString) "False"
  , Test "any isEven empty" (\() -> any isEven empty |> toString) "False"
  , Test "indexOf 3 (2 ::: 3 ::: 4 :::  empty)" (\() -> indexOf 3 (2 ::: 3 ::: 4 :::  empty) |> toString) "1"
  , Test "indexOf 0 (2 ::: 3 ::: 4 :::  empty)" (\() -> indexOf 0 (2 ::: 3 ::: 4 :::  empty) |> toString) "-1"
  , Test "indexOf 42 empty" (\() -> indexOf 42 empty |> toString) "-1"
  , Test "indexBy isEven (1 ::: 4 ::: 5 :::  empty)" (\() -> indexBy isEven (1 ::: 4 ::: 5 :::  empty) |> toString) "1"
  , Test "indexBy isEven (1 ::: 3 ::: 5 :::  empty)" (\() -> indexBy isEven (1 ::: 3 ::: 5 :::  empty) |> toString) "-1"
  , Test "indexBy isEven empty" (\() -> indexBy isEven empty |> toString) "-1"
  , Test "take 4 (numbers())" (\() -> take 4 (numbers()) |> lazyListToString) "[[[ 1, 2, 3, 4, Empty ]]]"
  , Test "take -1 (numbers())" (\() -> take -1 (numbers()) |> lazyListToString) "[[[ Empty ]]]"
  , Test "drop 4 (numbers())" (\() -> drop 4 (numbers()) |> lazyListToString) "[[[ 5, 6, 7, 8, 9, ... ]]]"
  , Test "drop -1 (numbers())" (\() -> drop -1 (numbers()) |> lazyListToString) "[[[ 1, 2, 3, 4, 5, ... ]]]"
  , Test "dropWhile ((>) 4) (numbers())" (\() -> dropWhile ((>) 4) (numbers()) |> lazyListToString) "[[[ 4, 5, 6, 7, 8, ... ]]]"
  , Test "dropWhile ((>) 4) (rang 1 3)" (\() -> dropWhile ((>) 4) (range 1 3) |> lazyListToString) "[[[ Empty ]]]"
  , Test "(1 ::: 2 ::: empty) +++ (3 ::: 4 ::: empty)" (\() -> (1 ::: 2 ::: empty) +++ (3 ::: 4 ::: empty) |> lazyListToString) "[[[ 1, 2, 3, 4, Empty ]]]"
  , Test "cycle (1 ::: 2 ::: empty)" (\() -> cycle (1 ::: 2 ::: empty) |> lazyListToString) "[[[ 1, 2, 1, 2, 1, ... ]]]"
  , Test "reverse (1 ::: 2 ::: 3 ::: empty)" (\() -> reverse (1 ::: 2 ::: 3 ::: empty) |> lazyListToString) "[[[ 3, 2, 1, Empty ]]]"
  , Test "intersperse 0 (rangeStep 1 3 2)" (\() -> intersperse 0 (rangeStep 1 3 2) |> lazyListToString) "[[[ 1, 0, 3, Empty ]]]"
  , Test "intersperse 0 (numbers())" (\() -> intersperse 0 (numbers()) |> lazyListToString) "[[[ 1, 0, 2, 0, 3, ... ]]]"
  , Test "interleave (rangeStep 42 44 2) (range 1 2)" (\() -> interleave (rangeStep 42 44 2) (range 1 2) |> lazyListToString) "[[[ 42, 1, 44, 2, Empty ]]]"
  , Test "interleave (rangeStep 42 44 2) (numbers())" (\() -> interleave (rangeStep 42 44 2) (numbers()) |> lazyListToString) "[[[ 42, 1, 44, 2, 3, ... ]]]"
  , Test "interleave (numbers()) (rangeStep 42 44 2)" (\() -> interleave (numbers()) (rangeStep 42 44 2) |> lazyListToString) "[[[ 1, 42, 2, 44, 3, ... ]]]"
  , Test "interleave (openRangeStep 42 2) (openRangeStep 1 2)" (\() -> interleave (openRangeStep 42 2) (openRangeStep 1 2) |> lazyListToString) "[[[ 42, 1, 44, 3, 46, ... ]]]"
  , Test "keepIf isEven (numbers())" (\() -> keepIf isEven (numbers()) |> lazyListToString) "[[[ 2, 4, 6, 8, 10, ... ]]]"
  , Test "keepIf isEven (range 1 5)" (\() -> keepIf isEven (range 1 5) |> lazyListToString) "[[[ 2, 4, Empty ]]]"
  , Test "dropIf isEven (numbers())" (\() -> dropIf isEven (numbers()) |> lazyListToString) "[[[ 1, 3, 5, 7, 9, ... ]]]"
  , Test "dropIf isEven (range 1 5)" (\() -> dropIf isEven (range 1 5) |> lazyListToString) "[[[ 1, 3, 5, Empty ]]]"
  , Test "unique (1 ::: 2 ::: 2 ::: 3 ::: empty)" (\() -> unique (1 ::: 2 ::: 2 ::: 3 ::: empty) |> lazyListToString) "[[[ 1, 2, 3, Empty ]]]"
  , Test "foldr (\\a b -> a + b) 0 (1 ::: 2 ::: 3 ::: empty)" (\() -> foldr (\a b -> a + b) 0 (1 ::: 2 ::: 3 ::: empty) |> toString) "6"
  , Test "flatten ((range 1 2) ::: (range 3 4) ::: empty)" (\() -> flatten ((range 1 2) ::: (range 3 4) ::: empty) |> lazyListToString) "[[[ 1, 2, 3, 4, Empty ]]]"
  , Test "scanl (+) 0 (1 ::: 2 ::: 3 ::: empty)" (\() -> scanl (+) 0 (1 ::: 2 ::: 3 ::: empty) |> lazyListToString) "[[[ 0, 1, 3, 6, Empty ]]]"
  , Test "scanl (+) 0 (numbers())" (\() -> scanl (+) 0 (numbers()) |> lazyListToString) "[[[ 0, 1, 3, 6, 10, ... ]]]"
  , Test "sum (1 ::: 2 ::: 3 ::: empty)" (\() -> sum (1 ::: 2 ::: 3 ::: empty) |> toString) "6"
  , Test "product (1 ::: 2 ::: 3 ::: empty)" (\() -> product (1 ::: 2 ::: 3 ::: empty) |> toString) "6"
  , Test "maximum (1 ::: 2 ::: 3 ::: empty)" (\() -> maximum (1 ::: 2 ::: 3 ::: empty) |> toString) "Just 3"
  , Test "maximum empty" (\() -> maximum empty |> toString) "Nothing"
  , Test "minimum (1 ::: 2 ::: 3 ::: empty)" (\() -> minimum (1 ::: 2 ::: 3 ::: empty) |> toString) "Just 1"
  , Test "minimum empty" (\() -> minimum empty |> toString) "Nothing"
  , Test "numbers() |> map ((*) 2)" (\() -> numbers() |> map ((*) 2) |> lazyListToString) "[[[ 2, 4, 6, 8, 10, ... ]]]"
  , Test "zip (numbers()) (numbers()) !!! 2" (\() -> zip (numbers()) (numbers()) !!! 2 |> toString) "Just (3,3)"
  , Test "zip3 (numbers()) (numbers()) (numbers()) !!! 2" (\() -> zip3 (numbers()) (numbers()) (numbers()) !!! 2 |> toString) "Just (3,3,3)"
  , Test "zip4 (numbers()) (numbers()) (numbers()) (numbers()) !!! 2" (\() -> zip4 (numbers()) (numbers()) (numbers()) (numbers()) !!! 2 |> toString) "Just (3,3,3,3)"
  , Test "zip5 (numbers()) (numbers()) (numbers()) (numbers()) (numbers()) !!! 2" (\() -> zip5 (numbers()) (numbers()) (numbers()) (numbers()) (numbers()) !!! 2 |> toString) "Just (3,3,3,3,3)"
  , Test "andMap (numbers()) (repeat ((*) 2))" (\() -> andMap (numbers()) (repeat ((*) 2)) |> lazyListToString) "[[[ 2, 4, 6, 8, 10, ... ]]]"
  , Test "andThen (\\a -> range a (a + 1)) (numbers())" (\() -> andThen (\a -> range a (a + 1)) (numbers()) |> lazyListToString) "[[[ 1, 2, 2, 3, 3, ... ]]]"
  , Test "partition (\\a -> a < 2) (0 ::: 1 ::: 2 ::: empty) |> Tuple.first" (\() -> partition (\a -> a < 2) (0 ::: 1 ::: 2 ::: empty) |> Tuple.first |> lazyListToString) "[[[ 0, 1, Empty ]]]"
  , Test "partition (\\a -> a < 2) (0 ::: 1 ::: 2 ::: empty) |> Tuple.second" (\() -> partition (\a -> a < 2) (0 ::: 1 ::: 2 ::: empty) |> Tuple.second |> lazyListToString) "[[[ 2, Empty ]]]"
  , Test "partition isEven (0 ::: 1 ::: 2 ::: empty) |> Tuple.first" (\() -> partition isEven (0 ::: 1 ::: 2 ::: empty) |> Tuple.first |> lazyListToString) "[[[ 0, 2, Empty ]]]"
  , Test "partition isEven (0 ::: 1 ::: 2 ::: empty) |> Tuple.second" (\() -> partition isEven (0 ::: 1 ::: 2 ::: empty) |> Tuple.second |> lazyListToString) "[[[ 1, Empty ]]]"
  , Test "unzip ((0, True) ::: (17, False) ::: (1337, True) ::: empty) |> Tuple.first" (\() -> unzip ((0, True) ::: (17, False) ::: (1337, True) ::: empty) |> Tuple.first |> lazyListToString) "[[[ 0, 17, 1337, Empty ]]]"
  , Test "unzip ((0, True) ::: (17, False) ::: (1337, True) ::: empty) |> Tuple.second" (\() -> unzip ((0, True) ::: (17, False) ::: (1337, True) ::: empty) |> Tuple.second |> lazyListToString) "[[[ True, False, True, Empty ]]]"
  , Test "filterMap isTeen (3 ::: 15 ::: 12 ::: 18 ::: 24 ::: empty)" (\() -> filterMap isTeen (3 ::: 15 ::: 12 ::: 18 ::: 24 ::: empty) |> lazyListToString) "[[[ 15, 18, Empty ]]]"
  , Test """indexedMap (,) ("Tom" ::: "Sue" ::: "Bob" ::: empty)""" (\() -> indexedMap (,) ("Tom" ::: "Sue" ::: "Bob" ::: empty) |> lazyListToString) """[[[ (0,"Tom"), (1,"Sue"), (2,"Bob"), Empty ]]]"""
  , Test "product2 (range 1 2) (range 3 4)" (\() -> product2 (range 1 2) (range 3 4) |> lazyListToString) "[[[ (1,3), (1,4), (2,3), (2,4), Empty ]]]"
  , Test "product3 (range 1 2) (range 3 4) (range 5 6) !!! 7" (\() -> product3 (range 1 2) (range 3 4) (range 5 6) !!! 7 |> toString) "Just (2,4,6)"
  , Test "product4 (range 1 2) (range 3 4) (range 5 6) (range 7 8) !!! 15" (\() -> product4 (range 1 2) (range 3 4) (range 5 6) (range 7 8) !!! 15 |> toString) "Just (2,4,6,8)"
  , Test "product5 (range 1 2) (range 3 4) (range 5 6) (range 7 8) (range 9 10) !!! 31" (\() -> product5 (range 1 2) (range 3 4) (range 5 6) (range 7 8) (range 9 10) !!! 31 |> toString) "Just (2,4,6,8,10)"
  , Test "toList (range 1 4)" (\() -> toList (range 1 4) |> toString) "[1,2,3,4]"
  , Test "fromList [ 1, 2, 3, 4 ]" (\() -> fromList [ 1, 2, 3, 4 ] |> lazyListToString) "[[[ 1, 2, 3, 4, Empty ]]]"
  , Test "toArray (range 1 4)" (\() -> toArray (range 1 4) |> toString) "Array.fromList [1,2,3,4]"
  , Test "fromArray (Array.fromList [ 1, 2, 3, 4 ])" (\() -> fromArray (Array.fromList [ 1, 2, 3, 4 ]) |> lazyListToString) "[[[ 1, 2, 3, 4, Empty ]]]"
  , Test "sort (range 4 1)" (\() -> sort (range 4 1) |> lazyListToString) "[[[ 1, 2, 3, 4, Empty ]]]"
  , Test "sortBy (\\a -> Tuple.second a) (('a',3) ::: ('b',1) ::: ('c',2) ::: empty)" (\() -> sortBy (\a -> Tuple.second a) (('a',3) ::: ('b',1) ::: ('c',2) ::: empty) |> lazyListToString) "[[[ ('b',1), ('c',2), ('a',3), Empty ]]]"
  , Test "sortWith (\\a b -> compare (Tuple.second b) (Tuple.second a)) (('a',3) ::: ('b',1) ::: ('c',2) ::: empty)" (\() -> sortWith (\a b -> compare (Tuple.second b) (Tuple.second a)) (('a',3) ::: ('b',1) ::: ('c',2) ::: empty) |> lazyListToString) "[[[ ('a',3), ('c',2), ('b',1), Empty ]]]"
  ]

getRandomList n =
  Random.step (Random.list n (Random.int 1 n)) (Random.initialSeed 42)
    |> \(list, _) -> list

benchmarks : List Test
benchmarks =
  [
    Test "range 1 100000 |> length" (\() -> range 1 100000 |> length |> toString) "100000"
  , Test "numbers() |> take 100000 |> length" (\() -> numbers() |> take 100000 |> length |> toString) "100000"
  , Test "numbers() |> drop 99999 |> head" (\() -> numbers() |> drop 99999 |> head |> toString) "Just 100000"
  , Test "numbers() |> takeWhile ((>=) 100000) |> length" (\() -> numbers() |> takeWhile ((>=) 100000) |> length |> toString) "100000"
  , Test "range 1 100000 |> foldl (\\_ n -> n + 1) 0" (\() -> range 1 100000 |> reduce (\_ n -> n + 1) 0 |> toString) "100000"
  , Test "range 1 100000 |> foldr (\\_ n -> n + 1) 0" (\() -> range 1 100000 |> reduce (\_ n -> n + 1) 0 |> toString) "100000"
  , Test "range 1 100000 |> toList |> fromList |> length" (\() -> range 1 100000 |> toList |> fromList |> length |> toString) "100000"
  , Test "range 1 100000 |> toList |> fromList" (\() -> range 1 100000 |> toList |> fromList |> lazyListToString) "[[[ 1, 2, 3, 4, 5, ... ]]]"
  , Test "range 1 100000 |> toArray |> fromArray |> length" (\() -> range 1 100000 |> toArray |> fromArray |> length |> toString) "100000"
  , Test "range 1 100000 |> toArray |> fromArray" (\() -> range 1 100000 |> toArray |> fromArray |> lazyListToString) "[[[ 1, 2, 3, 4, 5, ... ]]]"
  , Test "range 100000 1 |> sort |> length" (\() -> range 100000 1 |> sort |> length |> toString) "100000"
  , Test "range 100000 1 |> sort" (\() -> range 100000 1 |> sort |> lazyListToString) "[[[ 1, 2, 3, 4, 5, ... ]]]"
  , Test "getRandomList 100000 |> List.sort |> fromList" (\() -> getRandomList 100000|> List.sort |> fromList |> lazyListToString) "[[[ 2, 2, 3, 4, 6, ... ]]]"
  , Test "getRandomList 100000 |> fromList |> length" (\() -> getRandomList 100000 |> fromList |> length |> toString) "100000"
  , Test "getRandomList 100000 |> fromList" (\() -> getRandomList 100000 |> fromList |> lazyListToString) "[[[ 63472, 22672, 58118, 27515, 45517, ... ]]]"
  , Test "getRandomList 100000 |> fromList |> sort" (\() -> getRandomList 100000 |> fromList |> sort |> lazyListToString) "[[[ 2, 2, 3, 4, 6, ... ]]]"
  , Test "getRandomList 100000 |> fromList |> sort |> drop 99996" (\() -> getRandomList 100000 |> fromList |> sort |> drop 99996 |> lazyListToString) "[[[ 99996, 99996, 99997, 99999, Empty ]]]"
  ]

runTest : Test -> (Test, Bool)
runTest test =
  case test of
    Test _ testfunc expected ->
      (test, testfunc() == expected)

runBench : Test -> (Test, Bool, Int)
runBench test =
  case test of
    Test _ testfunc expected ->
      Native.DateNow.getMillis()
        |> \t -> (t, testfunc())
        |> \(t, ans) ->
          let elpsd = round (Native.DateNow.getMillis() - t) in
          (test, ans == expected, elpsd)

tableBorder : Attribute msg
tableBorder =
  attribute "border" "1"

statusPassStyle : Attribute msg
statusPassStyle =
  style
    [
      ("color", "green")
    ]

statusFailStyle : Attribute msg
statusFailStyle =
  style
    [
      ("color", "red")
    ]

toTestTableRow: (Test, Bool) -> Html msg
toTestTableRow (test, result) =
  case test of
    Test expression testfunc expected ->
      tr []
        [ td [ align "center" ] [ text <| expression ]
--        , td [ align "center" ] [ text <| testfunc() ] -- to see what was produced
        , td [ align "center" ] [ text expected ] -- or enable this to see expected.
        , if result then
            td [ align "center", statusPassStyle ] [ text "passed" ]
          else td [ align "center", statusFailStyle ] [ text "failed" ]
        ]

toBenchTableRow: (Test, Bool, Int) -> Html msg
toBenchTableRow (test, result, time) =
  case test of
    Test expression testfunc expected ->
      tr []
        [ td [ align "center" ] [ text <| expression ]
--        , td [ align "center" ] [ text <| testfunc() ] -- to see what was produced
        , td [ align "center" ] [ text expected ] -- or enable this to see expected.
        , if result then
            td [ align "center", statusPassStyle ] [ text "passed" ]
          else td [ align "center", statusFailStyle ] [ text "failed" ]
        , td [ align "center" ] [ text <| toString time ]
        ]

main : Html.Html msg
main =
  let criticalAnswers = List.map runTest criticals
      criticalResult = List.all (\(_, a) -> a) criticalAnswers in
  div []
    [ h1 [ align "center" ]
      [ text "Unit Tests for the Lazy.List Library Module" ]
    , h2 [ align "center" ]
      [ text "Critical Tests" ]
    , h3 []
      [ text """If any of the following tests fail, the rest of the tests
                 will be aborted:""" ]
    , table [ tableBorder ]
        ( List.concat
            [
              [ thead []
                  [ th [ align "center" ] [ text "Expression" ]
                  , th [ align "center" ] [ text "Expected" ]
                  , th [ align "center" ] [ text "Status" ]
                  ]
              ]
              , List.map toTestTableRow criticalAnswers
            ]
        )
    , if not criticalResult then
        h4 [ statusFailStyle ] [ text "Summary:  Some tests failed" ]
      else 
        let ordinaryAnswers = List.map runTest ordinarys
            ordinaryResult = List.all (\(_, a) -> a) ordinaryAnswers in
        div []
          [
            h4 [ statusPassStyle ] [ text "Summary:  All tests passed" ]
          , h2 [ align "center" ]
            [ text "Ordinary Tests" ]
          , h3 []
            [ text  """If any of the following tests fail,
                        the benchmarks will be aborted:""" ]
          , table [ tableBorder ]
              ( List.concat
                  [
                    [ thead []
                        [ th [ align "center" ] [ text "Expression" ]
                        , th [ align "center" ] [ text "Expected" ]
                        , th [ align "center" ] [ text "Status" ]
                        ]
                    ]
                    , List.map toTestTableRow ordinaryAnswers
                  ]
              )
          , if not ordinaryResult then
              h4 [ statusFailStyle ] [ text "Summary:  Some tests failed" ]
            else 
              let benchmarkAnswers = List.map runBench benchmarks
                  benchmarkResult = List.all (\(_, a, _) -> a) benchmarkAnswers
                  totalTime =
                    ( benchmarkAnswers
                        |> List.foldr (\(_, _, t) a -> a + t) 0 |> toString
                    ) ++ " milliseconds." in
              div []
                [
                  h4 [ statusPassStyle ] [ text "Summary:  All tests passed" ]
                , h2 [ align "center" ]
                  [ text "Benchmarks" ]
                , h3 []
                  [ text """The results of these benchmarks should be noted
                              before and after changes are made to ensure
                              performance is not degraded for a given
                              machine, browser, dependency versions,
                              etc.:""" ]
                , table [ tableBorder ]
                    ( List.concat
                        [
                          [ thead []
                              [ th [ align "center" ] [ text "Expression" ]
                              , th [ align "center" ] [ text "Expected" ]
                              , th [ align "center" ] [ text "Status" ]
                              , th [ align "center" ] [ text "Milliseconds" ]
                              ]
                          ]
                          , List.map toBenchTableRow benchmarkAnswers
                        ]
                    )
                , if not benchmarkResult then
                    h4 [ statusFailStyle ] [ text ("Summary:  Some tests failed in " ++ totalTime) ]
                  else 
                    h4 [ statusPassStyle ] [ text ("Summary: All tests passed in " ++ totalTime) ]
                ]
          ]
    ]
