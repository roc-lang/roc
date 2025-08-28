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
(block
  (block
    (uc "B")
  )
)
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
