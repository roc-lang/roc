# META
~~~ini
description=Empty string literal
type=expr
~~~
# SOURCE
~~~roc
""
~~~
# TOKENS
~~~text
String ~~~
# PARSE
~~~clojure
(str_literal_small "")
~~~
# FORMATTED
~~~roc
""
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.str_literal_small)
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
