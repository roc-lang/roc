# META
~~~ini
description=Lambda inside a collection
type=expr
~~~
# SOURCE
~~~roc
(
	|
		a,
		b,
	| {
		a + b
	},
	|a, b| {
		a - b
	},
)
~~~
# TOKENS
~~~text
OpenRound OpBar LowerIdent Comma LowerIdent Comma OpBar OpenCurly LowerIdent OpPlus LowerIdent CloseCurly Comma OpBar LowerIdent Comma LowerIdent OpBar OpenCurly LowerIdent OpBinaryMinus LowerIdent CloseCurly Comma CloseRound ~~~
# PARSE
~~~clojure
(lambda
  (body
    (tuple_literal
      (block
        (binop_plus
          (lc "a")
          (lc "b")
        )
      )
      (lambda
        (body
          (block
            (binop_minus
              (lc "a")
              (lc "b")
            )
          )
        )
        (args
          (lc "a")
          (lc "b")
        )
      )
    )
  )
  (args
    (lc "a")
    (lc "b")
  )
)
~~~
# FORMATTED
~~~roc
|a, b| ({
	a + b
}, |a, b| {
	a - b
})
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.lambda (canonicalized))
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
# No header found
~~~
