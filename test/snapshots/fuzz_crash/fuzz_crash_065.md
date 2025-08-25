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
(Expr.block
  (Expr.block
    (Expr.apply_tag)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "[]_others")
~~~
# TYPES
~~~roc
~~~
