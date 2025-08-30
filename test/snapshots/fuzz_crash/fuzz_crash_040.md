# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}{
o:0)
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly OpenCurly LowerIdent OpColon Int CloseRound ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "f")
      (binop_platform
        (str_literal_small "")
        (block)
      )
    )
))
~~~
# FORMATTED
~~~roc
app { f: "" platform [] }

{
	o : 0
	)
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:4 to 2:5

**Parse Error**
at 1:20 to 2:5

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.block
    (Expr.binop_colon
      (Expr.lookup "o")
      (Expr.num_literal_i32 0)
    )
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
