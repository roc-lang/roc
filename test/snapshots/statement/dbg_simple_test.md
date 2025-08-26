# META
~~~ini
description=Simple debug test to understand parsing behavior
type=file
~~~
# SOURCE
~~~roc
module [test]

test = {
    x = 42
    dbg(x)
}
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpAssign OpenCurly LowerIdent OpAssign Int KwDbg OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "test")
    (block
      (binop_equals
        (lc "x")
        (num_literal_i32 42)
      )
      (apply_anon
        (malformed malformed:expr_unexpected_token)
        (lc "x")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [
	test,
]

test = {
	x = 42
	dbg((x)
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
