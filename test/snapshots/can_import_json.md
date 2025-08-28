# META
~~~ini
description=Import with module-qualified usage
type=file
~~~
# SOURCE
~~~roc
module []

import json.Json

main = Json.utf8
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare KwImport LowerIdent Dot UpperIdent LowerIdent OpAssign UpperIdent Dot LowerIdent ~~~
# PARSE
~~~clojure
(block
  (import
    (binop_pipe
      (lc "json")
      (uc "Json")
    )
  )
  (binop_equals
    (lc "main")
    (binop_pipe
      (uc "Json")
      (dot_lc "utf8")
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

import json.Json
main = Json.utf8
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:1 to 3:17

**Unsupported Node**
at 5:8 to 5:12

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_equals
    (Expr.lookup "main")
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
main : _a
~~~
