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
    (lc "json")
    (uc "Json")
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
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_plus)
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
