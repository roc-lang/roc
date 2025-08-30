# META
~~~ini
description=A simple module with no blanks
type=file
~~~
# SOURCE
~~~roc
module [hello!, world]
import pf.Stdout
hello! = Stdout.line!("Hello")
world = "World"
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent OpBang Comma LowerIdent CloseSquare KwImport LowerIdent Dot UpperIdent LowerIdent OpBang OpAssign UpperIdent Dot LowerIdent OpBang OpenRound String CloseRound LowerIdent OpAssign String ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "hello")

    (lc "world")
))
~~~
# FORMATTED
~~~roc
module [hello, world]

import pf.Stdout
hello! = Stdout.line!("Hello")
world = "World"
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 2:1 to 2:17

**Unsupported Node**
at 3:10 to 3:16

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_equals
    (Expr.not_lookup)
    (Expr.apply_ident)
  )
  (Expr.binop_equals
    (Expr.lookup "world")
    (Expr.str_literal_big)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
world : Str
~~~
