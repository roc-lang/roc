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
~~~
# FORMATTED
~~~roc
module []


import json.Json
main = Json.utf8
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **Json.utf8** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_json.md:5:8:5:17:**
```roc
main = Json.utf8
```
       ^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
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
