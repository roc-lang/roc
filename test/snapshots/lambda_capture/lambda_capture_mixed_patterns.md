# META
~~~ini
description=Mixed capture patterns in block expression
type=expr
~~~
# SOURCE
~~~roc
(|base| {
		simple = |x| base + x + 1
		simple(1)
})(1)
~~~
# TOKENS
~~~text
OpenRound OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpPlus LowerIdent OpPlus Int LowerIdent OpenRound Int CloseRound CloseCurly CloseRound OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (lambda
    (body
      (block
        (binop_equals
          (lc "simple")
          (lambda
            (body
              (binop_plus
                (binop_plus
                  (lc "base")
                  (lc "x")
                )
                (num_literal_i32 1)
              )
            )
            (args
              (lc "x")
            )
          )
        )
        (apply_lc
          (lc "simple")
          (num_literal_i32 1)
        )
      )
    )
    (args
      (lc "base")
    )
  )
  (num_literal_i32 1)
)
~~~
# FORMATTED
~~~roc
(|base| {
	simple = |x| (base + x) + 1
	simple(1)
})(1)
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
(expr :tag apply_ident :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
