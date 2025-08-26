# META
~~~ini
description=Debug statement in body context
type=file
~~~
# SOURCE
~~~roc
module [main]

main = {
    x = 42
    dbg x
    x + 1
}
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpAssign OpenCurly LowerIdent OpAssign Int KwDbg LowerIdent LowerIdent OpPlus Int CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "main")
    (block
      (binop_equals
        (lc "x")
        (num_literal_i32 42)
      )
      (malformed malformed:expr_unexpected_token)
      (lc "x")
      (binop_plus
        (lc "x")
        (num_literal_i32 1)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [
	main,
]

main = {
	x = 42
	dbg
	x
	x + 1
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 5:5 to 5:5

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
