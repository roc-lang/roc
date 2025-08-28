# META
~~~ini
description=Import with exposing syntax test
type=file
~~~
# SOURCE
~~~roc
module [main]

import pf.Stdout exposing [line!, write!]

main = 42
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare LowerIdent OpBang Comma LowerIdent OpBang CloseSquare LowerIdent OpAssign Int ~~~
# PARSE
~~~clojure
(block
  (import
    (binop_exposing
      (binop_pipe
        (lc "pf")
        (uc "Stdout")
      )
      (list_literal
        (lc "line")
        (lc "write")
      )
    )
  )
  (binop_equals
    (lc "main")
    (num_literal_i32 42)
  )
)
~~~
# FORMATTED
~~~roc
module [main]

import pf.Stdout exposing [line, write]
main = 42
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:1 to 3:42

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_equals
    (Expr.lookup "main")
    (Expr.num_literal_i32 42)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
main : Num(_size)
~~~
