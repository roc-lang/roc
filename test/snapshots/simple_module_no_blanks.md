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
  (binop_equals
    (not_lc "hello")
    (apply_anon
      (binop_pipe
        (uc "Stdout")
        (not_lc "line")
      )
      (str_literal_big "Hello")
    )
  )
  (binop_equals
    (lc "world")
    (str_literal_big "World")
  )
)
~~~
# FORMATTED
~~~roc
module [
	hello,
	world,
]

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
at 3:1 to 3:7

**Unsupported Node**
at 3:10 to 3:16

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
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
