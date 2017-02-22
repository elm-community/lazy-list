# HTML Page Implementing Unit Tests and Some Benchmarks

The "Main.elm" program in this folder implements Unit Tests and
 a few benchmarks for the Lazy.List module.

It is intended to be run from a root project directory containing
 this entire cloned repository, then "cd" to the root project directory
 of the clone ("lazy-list").

The easiest way to use it is likely to open "elm-reactor" in the
 project (root) directory as above, open your browser to the gieven port,
 browse to this directory, then click on the "Main.elm" file in this
 directory to compile it and see the results of the testing as a HTML page.

 Alternately, run "elm-make tests/Main.elm --output tests.html" from the
  project root direcory, then open "tests.html" in your browser to view the
  same test results as a HTML page.

**Warning:  It can take 10 seconds or more to refresh the page while the
 tests are completed, so just be patient and wait**