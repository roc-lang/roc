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
    (lc "foo")
    (uc "BAR")
  )
)
~~~
# FORMATTED
~~~roc
module []

import json.Json
[foo, BAR]
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_plus)
  (Expr.binop_or)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
