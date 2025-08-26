# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]import
S
0
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare KwImport UpperIdent Int ~~~
# PARSE
~~~clojure
(block
  (import
    (uc "S")
  )
  (num_literal_i32 0)
)
~~~
# FORMATTED
~~~roc
module []

import S
0
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_plus)
  (Expr.binop_star)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
