# META
~~~ini
description=Unicode overflow (should error)
type=expr
~~~
# SOURCE
~~~roc
"\u(FFFFFF)"
~~~
# TOKENS
~~~text
String ~~~
# PARSE
~~~clojure
(str_literal_big "u(FFFFFF)")
~~~
# FORMATTED
~~~roc
"\u(FFFFFF)"
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
