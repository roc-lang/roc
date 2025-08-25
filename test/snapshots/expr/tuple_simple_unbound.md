# META
~~~ini
description=Simple tuple literal
type=expr
~~~
# SOURCE
~~~roc
(1, "hello", True)
~~~
# TOKENS
~~~text
OpenRound Int Comma String Comma UpperIdent CloseRound ~~~
# PARSE
~~~clojure
(tuple_literal
  (num_literal_i32 1)
  (str_literal_big "hello")
  (uc "True")
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 1:18 to 1:18

# CANONICALIZE
~~~clojure
(Expr.malformed)
~~~
# SOLVED
~~~clojure
(expr :tag malformed :type "Error")
~~~
# TYPES
~~~roc
Error
~~~
