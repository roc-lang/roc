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
NO CHANGE
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - inline_ingested_file.md:1:9:1:12
# PROBLEMS
**Parse Error**
at 3:21 to 3:21

**Unsupported Node**
at 3:21 to 3:21

**Pattern in Expression Context**
at 3:31 to 3:34

**Unsupported Node**
at 4:1 to 4:12

**Unsupported Node**
at 6:7 to 6:23

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.str_literal_big)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "data")
    (Expr.malformed)
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
