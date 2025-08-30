# META
~~~ini
description="An inner lambda captures a variable defined in an outer lambda's scope."
type=expr
~~~
# SOURCE
~~~roc
{
    f = (|a| |b| a + b)
    g = f(10)
    g(5) # Expect: 15
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpAssign OpenRound OpBar LowerIdent OpBar OpBar LowerIdent OpBar LowerIdent OpPlus LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpenRound Int CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "f")
    (lambda
      (body
        (lambda
          (body
            (binop_plus
              (lc "a")
              (lc "b")
            )
          )
          (args
            (lc "b")
          )
        )
      )
      (args
        (lc "a")
      )
    )
  )
  (binop_equals
    (lc "g")
    (apply_lc
      (lc "f")
      (num_literal_i32 10)
    )
  )
  (apply_lc
    (lc "g")
    (num_literal_i32 5)
  )
)
~~~
# FORMATTED
~~~roc
f = |a| |b| a + b
g = f(10)
g(5)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
    (Expr.lookup "f")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "g")
    (Expr.apply_ident)
  )
  (Expr.apply_ident)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~
