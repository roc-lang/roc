# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]{B
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare OpenCurly UpperIdent ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

{
	B
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:9 to 1:11

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
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
