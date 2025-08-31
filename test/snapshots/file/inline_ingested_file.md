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
    (Expr.apply_ident)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
