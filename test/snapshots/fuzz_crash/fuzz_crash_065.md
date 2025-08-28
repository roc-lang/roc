# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]{R}
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare OpenCurly UpperIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (block
    (uc "R")
  )
)
~~~
# FORMATTED
~~~roc
module []

{
	R
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
