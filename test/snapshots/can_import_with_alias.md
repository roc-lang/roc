# META
~~~ini
description=Import with explicit alias
type=file
~~~
# SOURCE
~~~roc
module []

import json.Json as MyJson

main = MyJson.decode
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine KwImport LowerIdent Dot UpperIdent KwAs UpperIdent BlankLine LowerIdent OpAssign UpperIdent Dot LowerIdent ~~~
# PARSE
~~~clojure
(module-header)
(block
  (import
    (binop_as
      (binop_dot
        (lc "json")
        (uc "Json")
      )
      (uc "MyJson")
    )
  )
  (binop_equals
    (lc "main")
    (binop_dot
      (uc "MyJson")
      (dot_lc "decode")
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

import json.Json as MyJson
main = (MyJson..decode)
~~~
# EXPECTED
MODULE NOT FOUND - can_import_with_alias.md:3:1:3:27
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**can_import_with_alias.md:5:1:5:5:**
```roc
main = MyJson.decode
```
^^^^


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
; Total type variables: 13
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 -> #10)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
~~~
# TYPES
~~~roc
main : _a
~~~
