# META
~~~ini
description=More davanced lambda capture
type=expr
~~~
# SOURCE
~~~roc
(|a, b, c| |x| a + b + c + x)(10, 20, 5)(7)
~~~
# TOKENS
~~~text
OpenRound OpBar LowerIdent Comma LowerIdent Comma LowerIdent OpBar OpBar LowerIdent OpBar LowerIdent OpPlus LowerIdent OpPlus LowerIdent OpPlus LowerIdent CloseRound OpenRound Int Comma Int Comma Int CloseRound OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (apply_anon
    (lambda
      (body
        (lambda
          (body
            (binop_plus
              (binop_plus
                (binop_plus
                  (lc "a")
                  (lc "b")
                )
                (lc "c")
              )
              (lc "x")
            )
          )
          (args
            (lc "x")
          )
        )
      )
      (args
        (tuple_literal
          (lc "a")
          (lc "b")
          (lc "c")
        )
      )
    )
    (tuple_literal
      (num_literal_i32 10)
      (num_literal_i32 20)
      (num_literal_i32 5)
    )
  )
  (num_literal_i32 7)
)
~~~
# FORMATTED
~~~roc
(|a, b, c| |x| ((a + b) + c) + x)((10, 20, 5))(7)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.apply_ident)
~~~
# SOLVED
~~~clojure
(expr :tag apply_ident :type "_d")
~~~
# TYPES
~~~roc
_d
~~~
