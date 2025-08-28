# META
~~~ini
description=inline_ingested_file
type=file
~~~
# SOURCE
~~~roc
module [foo]

import "users.json" as data : Str
import Json

foo = Json.parse(data)
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare KwImport String KwAs LowerIdent OpColon UpperIdent KwImport UpperIdent LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound ~~~
# PARSE
~~~clojure
(block
  (import
    (str_literal_big "users.json")
    (binop_colon
      (lc "data")
      (uc "Str")
    )
  )
  (import
    (uc "Json")
  )
  (binop_equals
    (lc "foo")
    (apply_anon
      (binop_pipe
        (uc "Json")
        (dot_lc "parse")
      )
      (lc "data")
    )
  )
)
~~~
# FORMATTED
~~~roc
module [foo]

import "users.json" as data : Str
import Json
foo = Json.parse(data)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_plus)
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
