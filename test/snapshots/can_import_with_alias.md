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
~~~
# FORMATTED
~~~roc
module []

import json.Json as MyJson

main = MyJson.decode
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **MyJson.decode** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_with_alias.md:5:8:5:21:**
```roc
main = MyJson.decode
```
       ^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.module_access
      (Expr.malformed)
      (Expr.malformed)
    )
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
