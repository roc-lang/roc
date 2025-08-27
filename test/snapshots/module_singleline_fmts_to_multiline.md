# META
~~~ini
description=An empty module with a singleline exposes with trailing comma
type=file
~~~
# SOURCE
~~~roc
module [something, SomeType,]
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma UpperIdent Comma CloseSquare ~~~
# PARSE
~~~clojure
(header-only)
~~~
# FORMATTED
~~~roc
module [something, SomeType]

~~~
# EXPECTED
NIL
# PROBLEMS
NIL
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
