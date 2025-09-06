# META
~~~ini
description=Simple string literal
type=expr
~~~
# SOURCE
~~~roc
"hello world"
~~~
# TOKENS
~~~text
String ~~~
# PARSE
~~~clojure
(str_literal_big "hello world")
~~~
# FORMATTED
~~~roc
"hello world"
~~~
# EXPECTED
NIL
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
