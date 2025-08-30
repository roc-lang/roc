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
(module-header)
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
**Unsupported Node**
at 1:9 to 2:2

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.num_literal_i32 0)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
