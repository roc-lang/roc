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
KwModule OpenSquare LowerIdent CloseSquare BlankLine KwImport String KwAs LowerIdent OpColon UpperIdent KwImport UpperIdent BlankLine LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "foo")
))
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
      (binop_dot
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
foo = Json..parse(data)
~~~
# EXPECTED
PARSE ERROR - inline_ingested_file.md:3:8:3:9
PARSE ERROR - inline_ingested_file.md:3:9:3:19
PARSE ERROR - inline_ingested_file.md:3:19:3:20
PARSE ERROR - inline_ingested_file.md:3:21:3:23
MODULE NOT FOUND - inline_ingested_file.md:4:1:4:12
UNDEFINED VARIABLE - inline_ingested_file.md:6:18:6:22
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **data** in this scope.
Is there an **import** or **exposing** missing up-top?

**inline_ingested_file.md:6:18:6:22:**
```roc
foo = Json.parse(data)
```
                 ^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.import)
  (Stmt.assign
    (pattern (Patt.ident "foo"))
    (Expr.fn_call)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 19
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 -> #15)
(var #11 _)
(var #12 _)
(var #13 -> #18)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 fn_pure)
~~~
# TYPES
~~~roc
~~~
