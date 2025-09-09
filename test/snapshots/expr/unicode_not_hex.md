# META
~~~ini
description=Unicode not hex (should error))
type=expr
~~~
# SOURCE
~~~roc
"abc\u(zzzz)def"
~~~
# TOKENS
~~~text
String ~~~
# PARSE
~~~clojure
(str_literal_big "abcu(zzzz)def")
~~~
# FORMATTED
~~~roc
"abc\u(zzzz)def"
~~~
# EXPECTED
INVALID UNICODE ESCAPE SEQUENCE - :0:0:0:0
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
