# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]_0={
)
 
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare Underscore Int OpAssign OpenCurly CloseRound ~~~
# PARSE
~~~clojure
(block
  (underscore)
  (binop_equals
    (num_literal_i32 0)
    (block
      (malformed malformed:expr_unexpected_token)
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

_0 = {
	<malformed>
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:1 to 2:1

**Parse Error**
at 1:12 to 3:2

**Pattern in Expression Context**
at 1:9 to 1:10

**Unsupported Node**
at 2:1 to 2:1

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
