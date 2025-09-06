# META
~~~ini
description=Import with module-qualified usage
type=file
~~~
# SOURCE
~~~roc
module []

import json.Json

main = Json.utf8
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine KwImport LowerIdent Dot UpperIdent BlankLine LowerIdent OpAssign UpperIdent Dot LowerIdent ~~~
# PARSE
~~~clojure
(module-header)
(block
  (import
    (binop_dot
      (lc "json")
      (uc "Json")
    )
  )
  (binop_equals
    (lc "main")
    (binop_dot
      (uc "Json")
      (dot_lc "utf8")
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

import json.Json
main = (Json..utf8)
~~~
# EXPECTED
MODULE NOT FOUND - can_import_json.md:3:1:3:17
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.record_access)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 11
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 -> #8)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
~~~
# TYPES
~~~roc
main : _a
~~~
