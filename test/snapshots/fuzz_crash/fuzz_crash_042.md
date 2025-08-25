# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]import u.R}g:r->R.a.E
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare KwImport LowerIdent Dot UpperIdent CloseCurly LowerIdent OpColon LowerIdent OpArrow UpperIdent Dot LowerIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(block
  (import
    (lc "u")
    (uc "R")
  )
  (malformed malformed:expr_unexpected_token)
  (binop_colon
    (lc "g")
    (binop_thin_arrow
      (lc "r")
      (binop_pipe
        (binop_pipe
          (uc "R")
          (dot_lc "a")
        )
        (uc "E")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

import u.R}
g: (r -> R.a | E)
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:19 to 1:19

**Unsupported Node**
at 1:9 to 1:19

**Unsupported Node**
at 1:19 to 1:19

**Unsupported Node**
at 1:22 to 1:29

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "g")
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
~~~
