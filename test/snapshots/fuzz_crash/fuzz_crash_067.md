# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module []

f = || {
    crash 1
}
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpAssign OpOr OpenCurly KwCrash Int CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "f")
    (malformed malformed:expr_unexpected_token)
  )
  (block
    (crash <statement>)
  )
)
~~~
# FORMATTED
~~~roc
module []

f = || 
{
	crash 1
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 3:5 to 3:8

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
    (Expr.lookup "f")
    (Expr.malformed)
  )
  (Expr.block
    (Expr.crash
      (Expr.num_literal_i32 1)
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
f : Error
~~~
