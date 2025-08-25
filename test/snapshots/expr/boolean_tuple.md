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
(True, False)
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 1:13 to 1:13

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
