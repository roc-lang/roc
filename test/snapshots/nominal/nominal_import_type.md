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
module [red]

import Color
red : Color.RGB
red = Color.RGB | Red
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:1 to 3:13

**Unsupported Node**
at 6:7 to 6:12

**Unsupported Node**
at 6:12 to 6:16

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "red")
    (Expr.module_access)
  )
  (Expr.binop_equals
    (Expr.lookup "red")
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
red : _a
~~~
