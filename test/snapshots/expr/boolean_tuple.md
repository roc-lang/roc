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
NIL
# CANONICALIZE
~~~clojure
(Expr.tuple_literal
  (Expr.tag_no_args)
  (Expr.tag_no_args)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 5
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 -> #4)
(var #4 tuple)
~~~
# TYPES
~~~roc
~~~
