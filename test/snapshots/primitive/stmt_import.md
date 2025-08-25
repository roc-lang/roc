# META
~~~ini
description=A primitive
type=file
~~~
# SOURCE
~~~roc
module []

import json.Json [foo, BAR]
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare KwImport LowerIdent Dot UpperIdent OpenSquare LowerIdent Comma UpperIdent CloseSquare ~~~
# PARSE
~~~clojure
(block
  (import
    (lc "json")
    (uc "Json")
  )
  (list_literal
    (tuple_literal
      (lc "foo")
      (uc "BAR")
    )
  )
)
~~~
# FORMATTED
~~~roc
module []


import json exposing [Json][(foo, BAR)]
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:1 to 3:17

**Unsupported Node**
at 3:18 to 3:27

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
