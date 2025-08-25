# META
~~~ini
description=Three-level nested lambda captures - innermost lambda captures from all outer levels
type=expr
~~~
# SOURCE
~~~roc
(|outer| |middle| |inner| outer + middle + inner)(1)(2)(3)
~~~
# TOKENS
~~~text
OpenRound OpBar LowerIdent OpBar OpBar LowerIdent OpBar OpBar LowerIdent OpBar LowerIdent OpPlus LowerIdent OpPlus LowerIdent CloseRound OpenRound Int CloseRound OpenRound Int CloseRound OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (apply_anon
    (apply_anon
      (lambda
        (body
          (lambda
            (body
              (lambda
                (body
                  (binop_plus
                    (binop_plus
                      (lc "outer")
                      (lc "middle")
                    )
                    (lc "inner")
                  )
                )
                (args
                  (lc "inner")
                )
              )
            )
            (args
              (lc "middle")
            )
          )
        )
        (args
          (lc "outer")
        )
      )
      (num_literal_i32 1)
    )
    (num_literal_i32 2)
  )
  (num_literal_i32 3)
)
~~~
# FORMATTED
~~~roc
\outer -> \middle -> \inner -> (outer + middle) + inner(1)(2)(3)
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 1:2 to 1:10

# CANONICALIZE
~~~clojure
(Expr.apply_ident)
~~~
# SOLVED
~~~clojure
(expr :tag apply_ident :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
