# META
~~~ini
description=fuzz crash, unterminated single quote
type=file
~~~
# SOURCE
~~~roc
mule []

#el
vavar t= '
~~~
# TOKENS
~~~text
LowerIdent OpenSquare CloseSquare LowerIdent LowerIdent OpAssign MalformedSingleQuoteUnclosed ~~~
# PARSE
~~~clojure
(block
  (lc "mule")
  (list_literal)
  (lc "vavar")
  (binop_equals
    (lc "t")
    (malformed malformed:expr_unexpected_token)
  )
)
~~~
# FORMATTED
~~~roc
mule
[]

#el
vavar
t = '
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 4:10 to 4:10

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.str_literal_big)
  (Expr.binop_or)
  (Expr.str_literal_big)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
