# META
~~~ini
description="A basic case where a lambda captures one variable from its immediate parent scope."
type=expr
~~~
# SOURCE
~~~roc
{
    x = 5
    y = (|_| x)(1)
    y
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpAssign Int LowerIdent OpAssign OpenRound OpBar Underscore OpBar LowerIdent CloseRound OpenRound Int CloseRound LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "x")
    (num_literal_i32 5)
  )
  (binop_equals
    (lc "y")
    (apply_anon
      (lambda
        (body
          (lc "x")
        )
        (args
          (underscore)
        )
      )
      (num_literal_i32 1)
    )
  )
  (lc "y")
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:10 to 3:14

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.lookup "y")
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
x : Num(_size)
y : _a
~~~
