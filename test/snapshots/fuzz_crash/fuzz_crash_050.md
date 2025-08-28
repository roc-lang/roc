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

_
0 = {
	
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:1 to 2:1

**Parse Error**
at 1:12 to 3:2

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
