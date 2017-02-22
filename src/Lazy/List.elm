module Lazy
    exposing
        ( Lazy
        , force
        , lazy
        , lazyFromValue
        , map
        , map2
        , map3
        , map4
        , map5
        , apply
        , andThen
        , fix
        )

{-| This library lets you delay a computation until later.

# Basics
@docs Lazy, lazy, lazyFromValue, force

# Mapping
@docs map, map2, map3, map4, map5

# Chaining
@docs apply, andThen

# Recursion
@docs fix
-}

import Native.Lazy


-- PRIMITIVES


{-| A wrapper around a value that will be lazily evaluated.
-}
type Lazy a
    = Evaluated a
    | Unevaluated (() -> a)


{-| Delay the evaluation of a value until later. For example, maybe we will
need to generate a very long list and find its sum, but we do not want to do
it unless it is absolutely necessary.

    lazySum : Lazy Int
    lazySum =
        lazy (\() -> sum [1..1000000])

Now we only pay for `lazySum` if we actually need it.
-}
lazy : (() -> a) -> Lazy a
lazy thunk =
    Native.Lazy.lazy thunk


{-| `lazyFromValue' Sets the created Lazy a to an already evaluated value.
For example, maybe we want to set the tail of a lazy list to an Empty node so
there is no need to defer the calculation as it is a simple constant:

    type LazyList a = Empty | Cons a (Lazy (LazyList a))
    shortLazyList : LazyList Int
    shortLazyList =
      Cons 1 <| lazyFromValue Empty

Now the tail of the shortLazyList is immediately available to
force without calling an evaluation functtion.
-}
lazyFromValue : a -> Lazy a
lazyFromValue v =
    Native.Lazy.lazyFromValue v


{-| Force the evaluation of a lazy value. This means we only pay for the
computation when we need it. Here is a rather contrived example.

    lazySum : Lazy Int
    lazySum =
        lazy (\() -> List.sum [1..1000000])
    sums : (Int, Int, Int)
    sums =
        (force lazySum, force lazySum, force lazySum)

We are forcing this computation three times. The cool thing is that the first
time you `force` a value, the result is stored. This means you pay the cost on
the first one, but all the rest are very cheap, basically just looking up a
value in memory.
-}
force : Lazy a -> a
force (Lazy thunk) =
    thunk ()



-- COMPOSING LAZINESS


{-| Lazily apply a function to a lazy value.

    lazySum : Lazy Int
    lazySum =
        map List.sum (lazy (\() -> [1..1000000]))

The resulting lazy value will create a big list and sum it up when it is
finally forced.
-}
map : (a -> b) -> Lazy a -> Lazy b
map f a =
    lazy (\() -> f (force a))


{-| Lazily apply a function to two lazy values.

    lazySum : Lazy Int
    lazySum =
        lazy (\() -> List.sum [1..1000000])
    lazySumPair : Lazy (Int, Int)
    lazySumPair =
        map2 (,) lazySum lazySum
-}
map2 : (a -> b -> result) -> Lazy a -> Lazy b -> Lazy result
map2 f a b =
    lazy (\() -> f (force a) (force b))


{-| -}
map3 : (a -> b -> c -> result) -> Lazy a -> Lazy b -> Lazy c -> Lazy result
map3 f a b c =
    lazy (\() -> f (force a) (force b) (force c))


{-| -}
map4 : (a -> b -> c -> d -> result) -> Lazy a -> Lazy b -> Lazy c -> Lazy d -> Lazy result
map4 f a b c d =
    lazy (\() -> f (force a) (force b) (force c) (force d))


{-| -}
map5 : (a -> b -> c -> d -> e -> result) -> Lazy a -> Lazy b -> Lazy c -> Lazy d -> Lazy e -> Lazy result
map5 f a b c d e =
    lazy (\() -> f (force a) (force b) (force c) (force d) (force e))


{-| Lazily apply a lazy function to a lazy value. This is pretty rare on its
own, but it lets you map as high as you want.

    map3 f a b == f `map` a `apply` b `apply` c

It is not the most beautiful, but it is equivalent and will let you create
`map9` quite easily if you really need it.
-}
apply : Lazy (a -> b) -> Lazy a -> Lazy b
apply f x =
    lazy (\() -> (force f) (force x))


{-| Lazily chain together lazy computations, for when you have a series of
steps that all need to be performed lazily. This can be nice when you need to
pattern match on a value, for example, when appending lazy lists:

    type LazyList a = Empty | Cons a (Lazy (LazyList a))
    cons : a -> Lazy (LazyList a) -> Lazy (LazyList a)
    cons v lazylist =
      Lazy.map (\x -> Cons v <| lazy (\() -> x)) lazylist
    append : Lazy (LazyList a) -> Lazy (LazyList a) -> Lazy (LazyList a)
    append list1 list2 =
      let
        appendi lazylist1 =
          let appendHelp ll1 =
            case ll1 of
              Empty -> list2
              Cons first rest ->
                cons first (appendi rest)
          in
            lazylist1 |> Lazy.andThen appendHelp
      in appendi list1

By using `andThen` we ensure that neither `lazyList1` nor `lazyList2` are forced
before they are needed. So as written, the `append` function delays the pattern
matching until later.
Note that although this is the way to write `cons' and `append' for a `Lazy (Lazylist a)`
so as to avoid "The Halting Problem" and make infinite lazy lists possible inside the wrapper,
which would otherwise cause stack overflow (or detection and an exception thrown),
the extra level of laziness of the outer `Lazy' wrapper costs much in terms of performance
due to the number of force/thunk/lazy chains of function calls/composition needed.
-}
andThen : (a -> Lazy b) -> Lazy a -> Lazy b
andThen callback a =
    lazy (\() -> force (callback (force a)))



-- FORMING RECURSION


{-| The shared lazy `fix' point function is a functional expression for recursion:
it is called "shared" because the result accumulates to the same binding as
is the argument to the function, thus making the binding recursive.
This "lazy" version only works with delayed execution functions as
the checks built into `force' will prevent recursive evaluation and
potential stack overflow, throwing an exception before that happens.
Common uses are with lazy lists with deferred execution in their structure.
For example, using a function that produces a lazy list (delayed execution) of
all the 32-bit `Int' natural numbers (with upper bounds check) code as follows::

    type LazyList a = Empty | Cons a (Lazy (LazyList a))
    plus1 ll =
      case ll of
        Empty -> Empty
        Cons hd tl ->
          if hd == ox7FFFFFFF then Empty else
          Cons (hd + 1) <| lazy <| \() -> plus1 <| force tl
    nat32fs() =
      fix <| plus1 << Cons -1

By using `fix' we get a recursive (lazy) chain of computations
producing the lazy list of all naturals in this case, although this is a
contrived case and there are more direct ways to accomplish this task.
-}
fix : (Lazy a -> a) -> a
fix f =
    let
        r =
            lazy <| \() -> f r
    in
        force r
