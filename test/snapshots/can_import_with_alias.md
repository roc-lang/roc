# META
~~~ini
description=Import with explicit alias
type=file
~~~
# SOURCE
~~~roc
module []

import json.Json as MyJson

main = MyJson.decode
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare KwImport LowerIdent Dot UpperIdent KwAs UpperIdent LowerIdent OpAssign UpperIdent Dot LowerIdent ~~~
# PARSE
~~~clojure
(block
  (import
    (lc "json")
    (uc "Json")
    (uc "MyJson")
  )
  (binop_equals
    (lc "main")
    (binop_pipe
      (uc "MyJson")
      (dot_lc "decode")
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
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
