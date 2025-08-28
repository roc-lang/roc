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
vavar
t = 
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 4:10 to 4:10

# CANONICALIZE
~~~clojure
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_a")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~
