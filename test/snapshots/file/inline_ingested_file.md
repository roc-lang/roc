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
    (binop_as
      (str_literal_big "users.json")
      (binop_colon
        (lc "data")
        (uc "Str")
      )
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
