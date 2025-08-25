# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[}0}.a
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseCurly Int CloseCurly Dot LowerIdent ~~~
# PARSE
~~~clojure
(block
  (num_literal_i32 0)
  (binop_pipe
    (malformed malformed:expr_unexpected_token)
    (dot_lc "a")
  )
)
~~~
# FORMATTED
~~~roc
module [
	<malformed>,
]

0
<malformed> | .a
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:8 to 1:8

**Parse Error**
at 1:1 to 1:9

**Parse Error**
at 1:10 to 1:10

**Unsupported Node**
at 1:10 to 1:10

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.num_literal_i32 0)
  (Expr.lambda)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_arg -> _ret")
~~~
# TYPES
~~~roc
~~~
