# META
~~~ini
description=Record with single field as an argument
type=expr
~~~
# SOURCE
~~~roc
(|{ x }| x )({ x: -10 })
~~~
# TOKENS
~~~text
OpenRound OpBar OpenCurly LowerIdent CloseCurly OpBar LowerIdent CloseRound OpenRound OpenCurly LowerIdent OpColon OpUnaryMinus Int CloseCurly CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (lambda
    (body
      (lc "x")
    )
    (args
      (block
        (binop_colon
          (lc "x")
          (lc "x")
        )
      )
    )
  )
  (block
    (binop_colon
      (lc "x")
      (unary_neg <unary>)
    )
  )
)
~~~
# FORMATTED
~~~roc
|{
	x : x
}| x({
	x : -10
})
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.binop_equals)
~~~
# SOLVED
~~~clojure
(expr :tag binop_equals :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
