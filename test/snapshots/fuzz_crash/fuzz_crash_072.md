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
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_a")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~
