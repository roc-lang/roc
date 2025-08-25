# META
~~~ini
description=Expected an open bracket for the header
type=file
~~~
# SOURCE
~~~roc
module
~~~
# TOKENS
~~~text
KwModule ~~~
# PARSE
~~~clojure
(header-only)
~~~
# FORMATTED
~~~roc
module [
]

~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:7

# CANONICALIZE
~~~clojure
(empty)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# No top-level expression found in file
~~~
