# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}{
o:0}0
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly OpenCurly LowerIdent OpColon Int CloseCurly Int ~~~
# PARSE
~~~clojure
(block
  (block
    (binop_colon
      (lc "o")
      (num_literal_i32 0)
    )
  )
  (num_literal_i32 0)
)
~~~
# FORMATTED
~~~roc
app
{
	f: "" platform [],
}

{
	o : 0
}0
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.block
    (Expr.malformed)
  )
  (Expr.binop_star)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
