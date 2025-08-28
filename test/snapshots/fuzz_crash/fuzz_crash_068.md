# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]({0})
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare OpenRound OpenCurly Int CloseCurly CloseRound ~~~
# PARSE
~~~clojure
(block
  (block
    (num_literal_i32 0)
  )
)
~~~
# FORMATTED
~~~roc
module []

{
	0
}
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
