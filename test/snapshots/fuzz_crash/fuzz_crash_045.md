# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
platform""requires{}{}exposes[]packages{}provides[
~~~
# TOKENS
~~~text
KwPlatform String KwRequires OpenCurly CloseCurly OpenCurly CloseCurly KwExposes OpenSquare CloseSquare KwPackages OpenCurly CloseCurly KwProvides OpenSquare ~~~
# PARSE
~~~clojure
(platform-header)
~~~
# FORMATTED
~~~roc
platform "" requires {} exposes []
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_045.md:2:1:2:1
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_provides_close_square**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_045.md:1:1:1:51:**
```roc
platform""requires{}{}exposes[]packages{}provides[
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(empty)
~~~
# SOLVED
~~~clojure
; Total type variables: 0
~~~
# TYPES
~~~roc
~~~
