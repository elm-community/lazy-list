# Will be deprecated in Elm 0.19

This library depends on [elm-lang/lazy-list](https://github.com/elm-lang/lazy), which makes use of native code to memoize in addition to performing delayed computation. See the comments there to understand why this is generally not what is needed for most use cases of lazy.

An alternative implementation that does not memoize, and does not depend on elm-lang/lazy can be found here:

<http://package.elm-lang.org/packages/eeue56/elm-lazy-list/latest>

# Lazy list implementation in Elm

Lazy lists are useful for large trees where you will only need one path. They power shrinking in elm-test.

Prior to 0.18, this repo was elm-lazy-list. Starting with the 0.18 release, the repo is renamed lazy-list and versioning resets to 1.0.0.
