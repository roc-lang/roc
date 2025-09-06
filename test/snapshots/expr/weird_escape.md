# META
~~~ini
description=Weird escape (should error)
type=expr
~~~
# SOURCE
~~~roc
"abc\qdef"
~~~
# TOKENS
~~~text
String ~~~
# PARSE
~~~clojure
(str_literal_big "abcqdef")
~~~
# FORMATTED
~~~roc
"abc\qdef"
~~~
# EXPECTED
INVALID ESCAPE SEQUENCE - :0:0:0:0
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.str_literal_big)
~~~
# SOLVED
~~~clojure
; Total type variables: 2
(var #0 _)
(var #1 Str)
~~~
# TYPES
~~~roc
~~~
