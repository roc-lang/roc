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
(module-header)
~~~
# FORMATTED
~~~roc
module []

_
0 = {
	)
 
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:1 to 3:2

**Parse Error**
at 1:12 to 3:2

**Pattern in Expression Context**
at 1:9 to 1:10

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_equals
    (Expr.num_literal_i32 0)
    (Expr.block
      (Expr.malformed)
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
