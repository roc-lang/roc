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
(module-header)
~~~
# FORMATTED
~~~roc
module []

import u.R}
g : r -> R.a | E
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:19 to 1:20

**Unsupported Node**
at 1:9 to 1:19

**Unsupported Node**
at 1:25 to 1:26

**Unsupported Node**
at 1:26 to 1:28

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "g")
    (Expr.binop_thin_arrow
      (Expr.lookup "r")
      (Expr.lambda)
    )
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
