# META
~~~ini
description=An empty module with singleline exposes
type=file
~~~
# SOURCE
~~~roc
module [something, SomeType]
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma UpperIdent CloseSquare ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "something")

    (uc "SomeType")
))
~~~
# FORMATTED
~~~roc
module [something, SomeType]
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - module_nonempty_single.md:1:9:1:18
EXPOSED BUT NOT DEFINED - module_nonempty_single.md:1:20:1:28
# PROBLEMS
NIL
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
