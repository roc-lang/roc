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
(block
  (import
    (lc "pf")
    (uc "Stdout")
  )
  (lc "hello")
  (unary_not <unary>)
  (binop_pipe
    (uc "Stdout")
    (dot_lc "line")
  )
  (unary_not <unary>)
  (binop_equals
    (lc "world")
    (str_literal_big "World")
  )
)
~~~
# FORMATTED
~~~roc
module [
	hello!, world
]

import pf exposing [Stdout]
hello<malformed>!
Stdout | .line"Hello"!
world = "World"
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 3:8 to 3:8

**Unsupported Node**
at 2:1 to 2:17

**Unsupported Node**
at 3:8 to 3:8

**Unsupported Node**
at 3:10 to 3:16

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.lookup "hello")
  (Expr.unary_not)
  (Expr.lambda)
  (Expr.unary_not)
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
