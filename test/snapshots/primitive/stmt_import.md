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
    (binop_pipe
      (lc "json")
      (uc "Json")
    )
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
