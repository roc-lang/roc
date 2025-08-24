# META
~~~ini
description=Example of importing a nominal tag union from another module
type=file
~~~
# SOURCE
~~~roc
module [red]

import Color

red : Color.RGB
red = Color.RGB.Red
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare KwImport UpperIdent LowerIdent OpColon UpperIdent Dot UpperIdent LowerIdent OpAssign UpperIdent Dot UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(block
  (import
    (uc "Color")
  )
  (binop_colon
    (lc "red")
    (binop_pipe
      (uc "Color")
      (uc "RGB")
    )
  )
  (binop_equals
    (lc "red")
    (binop_pipe
      (binop_pipe
        (uc "Color")
        (uc "RGB")
      )
      (uc "Red")
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
MODULE NOT FOUND - nominal_import_type.md:3:1:3:13
# PROBLEMS
**Unsupported Node**
at 3:1 to 3:13

**Unsupported Node**
at 5:7 to 5:12

**Unsupported Node**
at 6:7 to 6:12

**Unsupported Node**
at 6:12 to 6:15

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "red")
    (Expr.lambda)
  )
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
