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
(module-header
  (exposes
    (lc "test")
))
~~~
# FORMATTED
~~~roc
module [test]

test = {
	x = 42
	dbg(x)
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 5:5 to 5:8

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
    (Expr.lookup "test")
    (Expr.block
      (Expr.binop_equals
        (Expr.lookup "x")
        (Expr.num_literal_i32 42)
      )
      (Expr.apply_ident)
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
test : _a
~~~
