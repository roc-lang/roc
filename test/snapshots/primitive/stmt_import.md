# META
~~~ini
description=A primitive
type=file
~~~
# SOURCE
~~~roc
module []

import json.Json [foo, BAR]
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine KwImport LowerIdent Dot UpperIdent OpenSquare LowerIdent Comma UpperIdent CloseSquare ~~~
# PARSE
~~~clojure
(module-header)
(block
  (import
    (binop_pipe
      (lc "json")
      (uc "Json")
    )
  )
  (list_literal
    (lc "foo")
    (uc "BAR")
  )
)
~~~
# FORMATTED
~~~roc
module []

import json.Json
[foo, BAR]
~~~
# EXPECTED
PARSE ERROR - stmt_import.md:3:18:3:19
PARSE ERROR - stmt_import.md:3:19:3:22
PARSE ERROR - stmt_import.md:3:22:3:23
PARSE ERROR - stmt_import.md:3:27:3:28
MODULE NOT FOUND - stmt_import.md:3:1:3:17
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **json** in this scope.
Is there an **import** or **exposing** missing up-top?

**stmt_import.md:3:8:3:12:**
```roc
import json.Json [foo, BAR]
```
       ^^^^


**UNDEFINED VARIABLE**
Nothing is named **foo** in this scope.
Is there an **import** or **exposing** missing up-top?

**stmt_import.md:3:19:3:22:**
```roc
import json.Json [foo, BAR]
```
                  ^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Expr.list_literal)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 9
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
~~~
# TYPES
~~~roc
~~~
