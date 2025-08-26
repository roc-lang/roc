# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]({})(!{0})
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare OpenRound OpenCurly CloseCurly CloseRound OpenRound OpBang OpenCurly Int CloseCurly CloseRound ~~~
# PARSE
~~~clojure
(block
  (apply_anon
    (record_literal)
    (unary_not <unary>)
  )
)
~~~
# FORMATTED
~~~roc
module []

{  }(!{
	0
})
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
