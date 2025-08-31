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
KwModule OpenSquare LowerIdent CloseSquare BlankLine KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare LowerIdent OpBang Comma LowerIdent OpBang CloseSquare BlankLine LowerIdent OpAssign Int ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "main")
))
~~~
# FORMATTED
~~~roc
module [main]

import pf.Stdout exposing [line!, write!]

main = 42
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.assign
    (pattern (Patt.ident "main"))
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
~~~
