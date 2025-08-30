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
(module-header
  (exposes
    (lc "main")
))
~~~
# FORMATTED
~~~roc
module [main]

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
at 5:5 to 5:9

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
    (Expr.lookup "main")
    (Expr.block
      (Expr.binop_equals
        (Expr.lookup "x")
        (Expr.num_literal_i32 42)
      )
      (Expr.malformed)
      (Expr.lookup "x")
      (Expr.binop_plus
        (Expr.lookup "x")
        (Expr.num_literal_i32 1)
      )
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
main : _a
~~~
