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
MODULE NOT FOUND - can_import_with_alias.md:3:1:3:27
# PROBLEMS
**Unsupported Node**
at 3:1 to 3:27

**Unsupported Node**
at 5:8 to 5:14

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
