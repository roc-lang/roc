# META
~~~ini
description=Record as an argument
type=expr
~~~
# SOURCE
~~~roc
(|{ x, y }| x * y)({ x: 10, y: 20 })
~~~
# TOKENS
~~~text
OpenRound OpBar OpenCurly LowerIdent Comma LowerIdent CloseCurly OpBar LowerIdent OpStar LowerIdent CloseRound OpenRound OpenCurly LowerIdent OpColon Int Comma LowerIdent OpColon Int CloseCurly CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (lambda
    (body
      (binop_star
        (lc "x")
        (lc "y")
      )
    )
    (args
      (record_literal
        (binop_colon
          (lc "x")
          (lc "x")
        )
        (binop_colon
          (lc "y")
          (lc "y")
        )
      )
    )
  )
  (block
    (binop_colon
      (lc "x")
      (binop_colon
        (tuple_literal
          (num_literal_i32 10)
          (lc "y")
        )
        (num_literal_i32 20)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
\{ x: x, y: y } -> x * y({
	x: ((10, y): 20)
})
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 1:2 to 1:11

**Unsupported Node**
at 1:30 to 1:31

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
