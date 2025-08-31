# META
~~~ini
description=Variable scoping with var keyword
type=file
~~~
# SOURCE
~~~roc
module []

# This should cause an error - var not allowed at top level
var topLevelVar_ = 0
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LineComment KwVar LowerIdent OpAssign Int ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []


# This should cause an error - var not allowed at top level
var topLevelVar_ = 0
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
