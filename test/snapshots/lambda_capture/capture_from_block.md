# META
~~~ini
description="A lambda within a block expression captures a variable also defined within that block."
type=expr
~~~
# SOURCE
~~~roc
{
    a = 10
    b = (|_| a * 2)(5)
    b
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpAssign Int LowerIdent OpAssign OpenRound OpBar Underscore OpBar LowerIdent OpStar Int CloseRound OpenRound Int CloseRound LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "a")
    (num_literal_i32 10)
  )
  (binop_equals
    (lc "b")
    (apply_anon
      (lambda
        (body
          (binop_star
            (lc "a")
            (num_literal_i32 2)
          )
        )
        (args
          (underscore)
        )
      )
      (num_literal_i32 5)
    )
  )
  (lc "b")
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
at 3:10 to 3:23

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.lookup "b")
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
a : Num(_size)
b : Error
~~~
