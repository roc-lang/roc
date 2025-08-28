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
