# META
~~~ini
description=Test that True and False resolve to Bool type in a tuple
type=expr
~~~
# SOURCE
~~~roc
(True, False)
~~~
# TOKENS
~~~text
OpenRound UpperIdent Comma UpperIdent CloseRound ~~~
# PARSE
~~~clojure
(tuple_literal
  (uc "True")
  (uc "False")
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.binop_double_slash)
~~~
# SOLVED
~~~clojure
(expr :tag binop_double_slash :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
