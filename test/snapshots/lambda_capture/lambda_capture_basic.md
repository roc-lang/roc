# META
~~~ini
description=Basic lambda capture detection during canonicalization
type=expr
~~~
# SOURCE
~~~roc
(|x| |y| x + y)(1)(2)
~~~
# TOKENS
~~~text
OpenRound OpBar LowerIdent OpBar OpBar LowerIdent OpBar LowerIdent OpPlus LowerIdent CloseRound OpenRound Int CloseRound OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (apply_anon
    (lambda
      (body
        (lambda
          (body
            (binop_plus
              (lc "x")
              (lc "y")
            )
          )
          (args
            (lc "y")
          )
        )
      )
      (args
        (lc "x")
      )
    )
    (num_literal_i32 1)
  )
  (num_literal_i32 2)
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
at 1:2 to 1:6

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
