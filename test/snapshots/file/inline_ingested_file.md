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
  (str_literal_big "users.json")
  (malformed malformed:expr_unexpected_token)
  (binop_colon
    (lc "data")
    (uc "Str")
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
module [
	foo,
]

"users.json"<malformed>data: Str
import Json

foo = Json | .parse(data)
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 3:21 to 3:21

**Unsupported Node**
at 3:21 to 3:21

**Unsupported Node**
at 4:1 to 4:12

**Unsupported Node**
at 6:7 to 6:11

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.str_literal_big)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "data")
    (Expr.apply_tag)
  )
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
