# META
~~~ini
description=Block expression with two decls and final binop expr
type=expr
~~~
# SOURCE
~~~roc
{
    x = 42
    y = x + 1
    y * 2
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpAssign Int LowerIdent OpAssign LowerIdent OpPlus Int LowerIdent OpStar Int CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "x")
    (num_literal_i32 42)
  )
  (binop_equals
    (lc "y")
    (binop_plus
      (lc "x")
      (num_literal_i32 1)
    )
  )
  (binop_star
    (lc "y")
    (num_literal_i32 2)
  )
)
~~~
# FORMATTED
~~~roc
x = 42
y = x + 1
y * 2
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
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
_a
~~~
