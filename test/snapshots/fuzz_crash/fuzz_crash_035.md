# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]{
 
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare OpenCurly ~~~
# PARSE
~~~clojure
(block
  (block)
)
~~~
# FORMATTED
~~~roc
module []

{
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:9 to 2:2

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.block
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
